package ru.tinkoff.tschema.finagle.zioRouting
package impl

import cats.Monad
import cats.syntax.monoid._
import com.twitter.finagle.http
import izumi.reflect.Tag
import ru.tinkoff.tschema.finagle.{Rejection, Routed, RoutedPlus}
import zio.{ZEnvironment, ZIO}

private[finagle] class ZioRoutedInstance[R: Tag, E] extends RoutedPlus[ZIOHttp[R, E, *]] {
  private type F[a] = ZIOHttp[R, E, a]
  implicit private[this] val self: RoutedPlus[F] = this
  implicit private[this] val monad: Monad[F]     = zio.interop.catz.monadErrorInstance

  def matched: F[Int] = ZIO.serviceWith[ZRouting](_.matched)

  def withMatched[A](m: Int, fa: F[A]): F[A] =
    fa.provideSomeEnvironment(env => ZEnvironment[ZioRouting[R]](env.get[ZioRouting[R]].copy(matched = m)))

  def path: F[CharSequence]                 = ZIO.serviceWith[ZRouting](_.path)
  def request: F[http.Request]              = ZIO.serviceWith[ZRouting](_.request)
  def reject[A](rejection: Rejection): F[A] =
    Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

  def combineK[A](x: F[A], y: F[A]): F[A]                                    =
    catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))
  @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
    z.catchSome { case Fail.Rejected(xrs) => f(xrs) }

  @inline private[this] def throwRej[A](map: Rejection): F[A] = ZIO.fail(Fail.Rejected(map))
}
