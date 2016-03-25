package org.http4s
package aws

import java.security.MessageDigest
import java.nio.charset.{Charset => NioCharset, StandardCharsets}
import java.text.SimpleDateFormat
import java.time.Instant
import java.time.format.DateTimeFormatter.{BASIC_ISO_DATE, ISO_INSTANT}

import scalaz.concurrent.Task
import scalaz.stream._
import scodec.bits._
import scodec.interop.scalaz._

object AwsSigner {
  case class Key private (id: String, secret: String)(val bytes: ByteVector)

  object Key {
    def apply(id: String, secret: String): ParseResult[Key] =
      ByteVector.fromBase64(secret) match {
        case Some(bytes) => ParseResult.success(new Key(id, secret)(bytes))
        case None => ParseResult.fail(s"Secret was not Base64", secret)
      }
  }

  private val Method = "AWS4-HMAC-SHA256"
  private val Charset: NioCharset = StandardCharsets.UTF_8
}

class AwsSigner(key: AwsSigner.Key, zone: String, service: String) {
  import AwsSigner._

  private def hash(bv: ByteVector) = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    bv.grouped(1024 * 16) foreach { chunk =>
      digest.update(chunk.toByteBuffer)
    }

    ByteVector(digest.digest)
  }

  private def bytes(s: String) = ByteVector(s.getBytes(Charset))

  private def hmac(key: ByteVector, data: ByteVector) = {
    val algo = "HmacSHA256"
    val hmac = javax.crypto.Mac.getInstance(algo)

    hmac.init(new javax.crypto.spec.SecretKeySpec(key.toArray, algo))
    ByteVector(hmac.doFinal(data.toArray))
  }

  private def sign(string: String, date: Instant) = {
    val kSecret = bytes(s"AWS4${key.secret}")
    val kDate = hmac(kSecret, bytes(BASIC_ISO_DATE.format(date)))
    val kRegion = hmac(kDate, bytes(zone))
    val kService = hmac(kRegion, bytes(service))
    val kSigning = hmac(kService, bytes("aws4_request"))
    hmac(kSigning, bytes(string))
  }

  def apply(request: Request, date: Instant = Instant.now): Task[Request] = {

    request.body.runFoldMap(a => a) map { fullBody =>

      val headers = request.headers.put(
        Header("x-amz-date", ISO_INSTANT.format(date))
      )

      val headersToSign = headers.put(
        Header("Host", request.uri.host.map(_.toString).getOrElse(throw new IllegalArgumentException("need a Host")))
      ).toList sortBy { h =>
        h.name.toString.toLowerCase
      }

      val signedHeaders = headersToSign.map(header => header.name.toString.toLowerCase).mkString(";")

      val canonicalRequest = Seq(
        request.method.name,
        request.uri.path,
        request.queryString,
        headersToSign.map({ header =>
          s"${header.name.toString.toLowerCase}:${header.value.trim}\n"
        }).mkString(""),
        signedHeaders,
        hash(fullBody).toHex
      ) mkString "\n"


      val stringToSign = Seq(
        Method,
        ISO_INSTANT.format(date),
        BASIC_ISO_DATE.format(date) + s"/$zone/$service/aws4_request",
        hash(ByteVector(canonicalRequest.getBytes(Charset))).toHex
      ) mkString "\n"

      val auth = Seq(
        "Credential" -> s"${key.id}/${BASIC_ISO_DATE.format(date)}/$zone/$service/aws4_request",
        "SignedHeaders" -> signedHeaders,
        "Signature" -> sign(stringToSign, date).toHex
      ) map { case (k, v) => s"$k=$v" } mkString ", "

      request.copy(
        headers = (headers.put(Header("Authorization", s"$Method $auth"))),
        body = Process.emit(fullBody)
      )
    }
  }
}
