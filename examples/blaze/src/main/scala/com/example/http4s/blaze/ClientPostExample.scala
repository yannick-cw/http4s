package com.example.http4s.blaze

import cats.effect._
import org.http4s._
import org.http4s.client.blaze.{defaultClient => client}
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.io._

object ClientPostExample extends App with Http4sClientDsl[IO] {
  val req = POST(uri("https://duckduckgo.com/"), UrlForm("q" -> "http4s"))
  val responseBody = client[IO].expect[String](req)
  println(responseBody.unsafeRunSync())
}
