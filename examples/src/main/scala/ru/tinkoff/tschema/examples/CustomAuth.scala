package ru.tinkoff.tschema.examples
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsRejected
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Route}
import akka.http.scaladsl.server.directives.Credentials
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL.DSLAtomAuth
import shapeless.{HList, Witness}

import scala.collection.concurrent.TrieMap

object CustomAuth extends ExampleModule {
  override def route: Route         = MkRoute(api)(handler)
  override def swag: SwaggerBuilder = MkSwagger(api)(())

  implicit val auth = SpecialBearerAuth(Map("kriwda" -> "admin"))

  def api =
    tagPrefix('adminka) |>
      SpecialBearerAuth |> ((
      post |> body[BanUser]('banUser) |> operation('ban) |> $$[Result]
    ) <> (
      get |> operation('bans) |> $$[List[BanUser]]
    ))

  private val banned = TrieMap.empty[String, BanUser]

  object handler {
    def ban(banUser: BanUser, userId: String): Result = {
      banned.put(banUser.userToBan, banUser)
      Result(s"$userId ok")
    }
    def bans: List[BanUser] = banned.values.toList
  }
}

@derive(encoder, decoder, swagger)
final case class BanUser(userToBan: String, description: Option[String], ttl: Option[Long])

@derive(encoder, decoder, swagger)
final case class Result(message: String)

final case class SpecialBearerAuth(values: Map[String, String])

object SpecialBearerAuth extends DSLAtomAuth {
  import Serve.{Add, serveAdd}

  implicit val swagger: SwaggerMapper[this.type] =
    (queryParam[String]('userId) |> bearerAuth('kriwda, 'kriwda)).swaggerMapper.as[this.type]

  import akka.http.scaladsl.server.Directives._
  implicit def serve[In <: HList](implicit auth: SpecialBearerAuth): Add[this.type, In, Witness.`'userId`.T, String] =
    serveAdd(parameter('userId).flatMap(userId =>
      authenticateOAuth2PF("kriwda", {
        case cred @ Credentials.Provided(_) if auth.values.get(userId).forall(cred.verify) => userId
      })))
}
