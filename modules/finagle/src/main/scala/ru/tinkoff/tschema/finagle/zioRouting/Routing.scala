package ru.tinkoff.tschema.finagle
package zioRouting

import cats.ApplicativeThrow
import cats.data.ReaderT
import cats.effect.std.Dispatcher
import cats.effect.Async
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._
import cats.syntax.semigroup._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.finagle.Rejection.{OptRecover, Recover}
import ru.tinkoff.tschema.finagle.zioRouting.Routing.FHttp
import ru.tinkoff.tschema.finagle.{ConvertService, LiftHttp, Rejection, Routed, RoutedPlus, RunHttp}
import ru.tinkoff.tschema.utils.SubString

final case class Routing(
    request: http.Request,
    path: CharSequence,
    matched: Int
)

object Routing extends FInstanceDecl {

  type FHttp[F[_], A] = ReaderT[F, Routing, A]

  implicit def routed[F[_]: Async, R, E]
      : RoutedPlus[FHttp[F, *]] with ConvertService[FHttp[F, *]] with LiftHttp[FHttp[F, *], F] =
    new FRoutedConvert[F]

  implicit def envRunnable[F[_]: Async](implicit
      optRecover: OptRecover[FHttp[F, *]] = OptRecover.default[FHttp[F, *]]
  ): RunHttp[FHttp[F, *], F] = {
    implicit val recover: Recover[FHttp[F, *]] = optRecover.orDefault
    zioResponse => execResponse(zioResponse)
  }

  private[this] def execResponse[F[_]: Async: ApplicativeThrow](envResponse: FHttp[F, Response])(implicit
      recover: Recover[FHttp[F, *]]
  ): F[Service[Request, Response]] = {
    Dispatcher.parallel[F].use { dispatcher =>
      Async[F].delay { request =>
        val promise = Promise[Response]()
        val routing = Routing(request, SubString(request.path), 0)

        dispatcher.unsafeToFuture(
          envResponse.recoverWith { case Rejected(rej) => recover(rej) }.run(routing).attempt.flatMap {
            case Right(res) => Async[F].pure(promise.setValue(res))
            case Left(ex)   =>
              Async[F].pure {
                val resp = Response(Status.InternalServerError)
                resp.setContentString(ex.getMessage)
                promise.setValue(resp)
              }
          }
        )
        promise
      }
    }
  }

}

private[finagle] class FInstanceDecl {

  private def cachedMonadInstance[G[_]: Async] = Async[FHttp[G, *]]

  protected class FRoutedConvert[G[_]: Async]
      extends RoutedPlus[FHttp[G, *]] with ConvertService[FHttp[G, *]] with LiftHttp[FHttp[G, *], G] {
    private type F[a] = FHttp[G, a]
    implicit private[this] val self: RoutedPlus[F] = this
    implicit private[this] val cached: Async[F]    = cachedMonadInstance[G]

    def matched: F[Int] = ReaderT(_.matched.pure[G])

    def withMatched[A](m: Int, fa: F[A]): F[A] = fa.local(_.copy(matched = m))

    def path: F[CharSequence]                 = ReaderT(_.path.pure[G])
    def request: F[http.Request]              = ReaderT(_.request.pure[G])
    def reject[A](rejection: Rejection): F[A] =
      Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

    def combineK[A](x: F[A], y: F[A]): F[A] =
      catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))

    def convertService[A](svc: Service[http.Request, A]): F[A] =
      ReaderT(r =>
        Async[G].async_(cb =>
          svc(r.request).respond {
            case twitter.util.Return(a) => cb(Right(a))
            case twitter.util.Throw(ex) => cb(Left(ex))
          }
        )
      )

    def apply[A](fa: G[A]): F[A] = ReaderT.liftF(fa)

    @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
      z.recoverWith { case Rejected(xrs) => f(xrs) }

    @inline private[this] def throwRej[A](map: Rejection): F[A] =
      cached.raiseError(Rejected(map))
  }

}
