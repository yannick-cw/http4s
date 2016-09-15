package org.http4s
package server
package middleware

import org.http4s.headers._
import scalaz._, Scalaz._
import scalaz.concurrent.Task

package object authentication {
  def challenge[A](challenge: Service[Request, Challenge \/ AuthedRequest[A]])
                  (service: AuthedService[A]): HttpService =
    Service.lift { req =>
      challenge(req) flatMap {
        case \/-(authedRequest) =>
          service(authedRequest)
        case -\/(challenge) =>
          Task.now(Response(Status.Unauthorized).putHeaders(`WWW-Authenticate`(challenge)))
      }
    }

  type BasicAuthStore[A] = Service[BasicCredentials, AuthReply[A]]

  /**
    * Provides Basic Authentication from RFC 2617.
    * @param realm The realm used for authentication purposes.
    * @param store Maps basic credentials to an auth reply
    */
  def basicAuth[A](realm: String, store: BasicAuthStore[A]): AuthMiddleware[A] = {
    val checkAuth: Service[Request, (Request, AuthReply[A])] =
      Service.lift { req: Request =>
        (req.headers.get(Authorization) match {
          case Some(Authorization(creds: BasicCredentials)) =>
            store(creds)
          case Some(Authorization(_)) =>
            Task.now(AuthFailed)
          case None =>
            Task.now(AuthFailed)
        }).map((req, _))
      }

    challenge(checkAuth map {
      case (req, AuthOk(user)) =>
        AuthedRequest(user, req).right
      case (_, AuthFailed) =>
        Challenge("Basic", realm, Map.empty).left
    })
  }
}
 
