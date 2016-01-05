package scalax.concurrent

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag
import scala.util.Try

/**
  * Represents an asynchronous computation that can be canceled
  * as long as it isn't complete.
  */
trait CancelableFuture[+T] extends Future[T] with Cancelable {

  // Overriding methods for getting CancelableFuture in return

  override def failed: CancelableFuture[Throwable] =
    CancelableFuture.wrap(this).failed
  override def transform[S](s: (T) => S, f: (Throwable) => Throwable)(implicit executor: ExecutionContext): CancelableFuture[S] =
    CancelableFuture.wrap(this).transform(s, f)
  override def map[S](f: (T) => S)(implicit executor: ExecutionContext): CancelableFuture[S] =
    CancelableFuture.wrap(this).map(f)
  override def flatMap[S](f: (T) => Future[S])(implicit executor: ExecutionContext): CancelableFuture[S] =
    CancelableFuture.wrap(this).flatMap(f)
  override def filter(p: (T) => Boolean)(implicit executor: ExecutionContext): CancelableFuture[T] =
    CancelableFuture.wrap(this).filter(p)
  override def collect[S](pf: PartialFunction[T, S])(implicit executor: ExecutionContext): CancelableFuture[S] =
    CancelableFuture.wrap(this).collect(pf)
  override def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): CancelableFuture[U] =
    CancelableFuture.wrap(this).recover(pf)
  override def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): CancelableFuture[U] =
    CancelableFuture.wrap(this).recoverWith(pf)
  override def zip[U](that: Future[U]): CancelableFuture[(T, U)] =
    CancelableFuture.wrap(this).zip(that)
  override def fallbackTo[U >: T](that: Future[U]): CancelableFuture[U] =
    CancelableFuture.wrap(this).fallbackTo(that)
  override def mapTo[S](implicit tag: ClassTag[S]): CancelableFuture[S] =
    CancelableFuture.wrap(this).mapTo(tag)
  override def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): CancelableFuture[T] =
    CancelableFuture.wrap(this).andThen(pf)
}

object CancelableFuture {
  /** Builder for a [[CancelableFuture]].
    *
    * N.B. The behavior on `cancelable.cancel()` should be for the `underlying`
    * future to be failed with `scala.concurrent.CancellationException`.
    * Also, canceling an already complete future should do nothing.
    *
    * @param underlying is an underlying `Future` reference that will respond to `onComplete` calls
    * @param cancelable is a [[Cancelable]] that can be used to cancel the active computation
    *                   and that will complete the future with a `CancellationException`
    */
  def apply[T](underlying: Future[T], cancelable: Cancelable): CancelableFuture[T] =
    new Implementation[T](underlying, cancelable)

  /** Internal; wraps any cancelable future `ref` into an [[Implementation]] */
  private def wrap[T](ref: CancelableFuture[T]): Implementation[T] =
    ref match {
      case _: Implementation[_] => ref.asInstanceOf[Implementation[T]]
      case _ => new Implementation[T](ref, ref)
    }

  /** An actual [[CancelableFuture]] implementation; internal. */
  private final class Implementation[+T](underlying: Future[T], cancelable: Cancelable)
    extends CancelableFuture[T] {

    def onComplete[U](f: (Try[T]) => U)(implicit executor: ExecutionContext): Unit =
      underlying.onComplete(f)(executor)
    def isCompleted: Boolean =
      underlying.isCompleted
    def value: Option[Try[T]] =
      underlying.value

    @throws[Exception](classOf[Exception])
    def result(atMost: Duration)(implicit permit: CanAwait): T =
      underlying.result(atMost)(permit)

    @throws[InterruptedException](classOf[InterruptedException])
    @throws[TimeoutException](classOf[TimeoutException])
    def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
      underlying.ready(atMost)(permit)
      this
    }

    def cancel(): Boolean =
      cancelable.cancel()

    // Overriding methods for getting CancelableFuture in return

    override def failed: CancelableFuture[Throwable] =
      new Implementation(underlying.failed, cancelable)
    override def transform[S](s: (T) => S, f: (Throwable) => Throwable)(implicit executor: ExecutionContext): CancelableFuture[S] =
      new Implementation(underlying.transform(s, f), cancelable)
    override def map[S](f: (T) => S)(implicit executor: ExecutionContext): CancelableFuture[S] =
      new Implementation(underlying.map(f), cancelable)
    override def flatMap[S](f: (T) => Future[S])(implicit executor: ExecutionContext): CancelableFuture[S] =
      new Implementation(underlying.flatMap(f), cancelable)
    override def filter(p: (T) => Boolean)(implicit executor: ExecutionContext): CancelableFuture[T] =
      new Implementation(underlying.filter(p), cancelable)
    override def collect[S](pf: PartialFunction[T, S])(implicit executor: ExecutionContext): CancelableFuture[S] =
      new Implementation(underlying.collect(pf), cancelable)
    override def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): CancelableFuture[U] =
      new Implementation(underlying.recover(pf), cancelable)
    override def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): CancelableFuture[U] =
      new Implementation(underlying.recoverWith(pf), cancelable)
    override def zip[U](that: Future[U]): CancelableFuture[(T, U)] =
      new Implementation(underlying.zip(that), cancelable)
    override def fallbackTo[U >: T](that: Future[U]): CancelableFuture[U] =
      new Implementation(underlying.fallbackTo(that), cancelable)
    override def mapTo[S](implicit tag: ClassTag[S]): CancelableFuture[S] =
      new Implementation(underlying.mapTo[S], cancelable)
    override def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): CancelableFuture[T] =
      new Implementation(underlying.andThen(pf), cancelable)
  }
}