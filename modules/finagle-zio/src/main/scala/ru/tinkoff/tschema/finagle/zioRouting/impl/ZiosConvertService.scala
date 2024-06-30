package ru.tinkoff.tschema.finagle.zioRouting
package impl

import com.twitter
import com.twitter.finagle.Service
import com.twitter.finagle.http.Request
import izumi.reflect.Tag
import ru.tinkoff.tschema.finagle.ConvertService
import zio.{UIO, ZIO}

private[finagle] class ZiosConvertService[R, E] extends ConvertService[ZIOH[R, E, *]] {
  def convertService[A](svc: Service[Request, A]): ZIOH[R, E, A] =
    ZIO.serviceWithZIO[ZRouting] { r =>
      ZIO.asyncInterrupt[R with ZRouting, Fail[E], A] { cb =>
        val fut = svc(r.request).respond {
          case twitter.util.Return(a) => cb(ZIO.succeed(a))
          case twitter.util.Throw(ex) => cb(ZIO.die(ex))
        }

        Left(ZIO.succeed(fut.raise(new InterruptedException)))
      }
    }
}
