package org.http4s
package argonaut

import _root_.argonaut.{DecodeResult => _, _}
import org.http4s.headers.`Content-Type`

trait ArgonautInstances {
  implicit val jsonDecoder: EntityDecoder[Json] = jawn.jawnDecoder(Parser.facade)

  def jsonOf[A](implicit decoder: DecodeJson[A]): EntityDecoder[A] =
    jsonDecoder.flatMapR { json =>
      decoder.decodeJson(json).fold(
        (message, history) =>
          DecodeResult.failure(InvalidMessageBodyFailure(s"Could not decode JSON: $json, error: $message, cursor: $history")),
        DecodeResult.success(_)
      )
    }

  implicit def jsonEncoder(implicit pp: PrettyParams = Argonaut.nospace): EntityEncoder[Json] =
    EntityEncoder.stringEncoder(Charset.`UTF-8`).contramap[Json] { json =>
      // TODO naive implementation materializes to a String.
      // Look into replacing after https://github.com/non/jawn/issues/6#issuecomment-65018736
      pp.pretty(json)
    }.withContentType(`Content-Type`(MediaType.`application/json`, Charset.`UTF-8`))

  def jsonEncoderOf[A](implicit encoder: EncodeJson[A], pp: PrettyParams = Argonaut.nospace): EntityEncoder[A] =
    jsonEncoder.contramap[A](encoder.encode)
}
