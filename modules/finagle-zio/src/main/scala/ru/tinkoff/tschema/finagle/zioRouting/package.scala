package ru.tinkoff.tschema.finagle
import com.twitter.finagle.http.{Response, Status}
import com.twitter.util.{Future, Promise}
import zio.{Exit, Fiber, Runtime, Unsafe, ZEnvironment, ZIO}

package object zioRouting {
  type NoError <: Nothing
  type None >: Any

  type ZRouting = ZioRouting[Any]

  type UIOHttp[+A]         = ZIO[ZioRouting[None], Fail[NoError], A]
  type IOHttp[+E, +A]      = ZIO[ZioRouting[None], Fail[E], A]
  type TaskHttp[+A]        = ZIO[ZioRouting[None], Fail[Throwable], A]
  type URIOHttp[-R, +A]    = ZIO[ZioRouting[R], Fail[NoError], A]
  type RIOHttp[-R, +A]     = ZIO[ZioRouting[R], Fail[Throwable], A]
  type ZIOHttp[-R, +E, +A] = ZIO[ZioRouting[R], Fail[E], A]

  type UIOH[+A]         = ZIO[ZRouting, Fail[NoError], A]
  type IOH[+E, +A]      = ZIO[ZRouting, Fail[E], A]
  type TaskH[+A]        = ZIO[ZRouting, Fail[Throwable], A]
  type URIOH[-R, +A]    = ZIO[ZRouting with R, Fail[NoError], A]
  type RIOH[-R, +A]     = ZIO[ZRouting with R, Fail[Throwable], A]
  type ZIOH[-R, +E, +A] = ZIO[ZRouting with R, Fail[E], A]

  private[zioRouting] def execWithRuntime[R, E <: Throwable](runtime: Runtime[R])(
      zio: ZIO[R, E, Response]
  ): Future[Response] = {
    val promise = Promise[Response]()

    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe
        .fork(setInterruption(zio, promise, runtime))
        .unsafe
        .addObserver {
          case Exit.Success(resp)  => promise.setValue(resp)
          case Exit.Failure(cause) =>
            val resp  = Response(Status.InternalServerError)
            val error = cause.squash
            resp.setContentString(Option(error.getLocalizedMessage).getOrElse(error.toString))
            promise.setValue(resp)
        }
    }

    promise
  }

  private def setInterruption[R, E, A, X](zio: ZIO[R, E, A], promise: Promise[X], rt: Runtime[Any]): ZIO[R, E, A] = {
    def setInterrupt(fiber: Fiber[Any, Any]) =
      ZIO.succeed(promise.setInterruptHandler { case _ =>
        Unsafe.unsafe { implicit unsafe =>
          rt.unsafe
            .fork(fiber.interrupt)
            .unsafe
            .addObserver(_ => ())
        }
      })

    zio.fork.tap(setInterrupt).flatMap(_.join)
  }

  private[zioRouting] def execResponse[R, R1, E <: Throwable](
      runtime: zio.Runtime[R],
      zioResponse: ZIO[R1, E, Response],
      f: ZEnvironment[R] => ZEnvironment[R1]
  ): Future[Response] =
    zioRouting.execWithRuntime(runtime)(
      zioResponse.provideSomeEnvironment[R](f)
    )
}
