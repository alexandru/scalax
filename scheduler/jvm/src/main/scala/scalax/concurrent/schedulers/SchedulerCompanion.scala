package scalax.concurrent.schedulers

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.{Executors, ScheduledExecutorService, ThreadFactory}
import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool
import scalax.concurrent.UncaughtExceptionReporter._
import scalax.concurrent.atomic.Atomic
import scalax.concurrent.{Scheduler, UncaughtExceptionReporter}


private[concurrent] abstract class SchedulerCompanion {
  /**
    * [[Scheduler]] builder.
    *
    * @param executor is the `ScheduledExecutorService` that handles the scheduling
    *                 of tasks into the future.
    *
    * @param ec is the execution context in which all tasks will run.
    * @param r is the [[UncaughtExceptionReporter]] that logs uncaught exceptions.
    */
  def apply(executor: ScheduledExecutorService, ec: ExecutionContext, r: UncaughtExceptionReporter): Scheduler = {
    AsyncScheduler(executor, ec, r)
  }

  /**
    * [[Scheduler]] builder.
    *
    * @param executor is the `ScheduledExecutorService` that handles the scheduling
    *                 of tasks into the future.
    *
    * @param ec is the execution context in which all tasks will run.
    */
  def apply(executor: ScheduledExecutorService, ec: ExecutionContext): Scheduler = {
    AsyncScheduler(executor, ec, UncaughtExceptionReporter(ec.reportFailure))
  }

  /**
    * [[Scheduler]] builder - uses Scalax's default `ScheduledExecutorService` for
    * handling the scheduling of tasks.
    *
    * @param ec is the execution context in which all tasks will run.
    * @param r is the [[UncaughtExceptionReporter]] that logs uncaught exceptions.
    */
  def apply(ec: ExecutionContext, r: UncaughtExceptionReporter): Scheduler =
    AsyncScheduler(defaultScheduledExecutor, ec, r)

  /**
    * [[Scheduler]] builder - uses Scalax's default `ScheduledExecutorService` for
    * handling the scheduling of tasks.
    *
    * @param ec is the execution context in which all tasks will run.
    */
  def apply(ec: ExecutionContext): Scheduler =
    AsyncScheduler(
      defaultScheduledExecutor, ec,
      UncaughtExceptionReporter(ec.reportFailure)
    )

  /**
    * Creates a [[Scheduler]] meant for computational heavy tasks.
    *
    * Characteristics:
    *
    * - backed by Scala's `ForkJoinPool` for the task execution, in async mode
    * - uses Scalax's default `ScheduledExecutorService` instance for scheduling
    * - all created threads are daemonic
    * - cooperates with Scala's `BlockContext`
    *
    * @param parallelism is the number of threads that can run in parallel
    * @param r is the [[UncaughtExceptionReporter]] that logs uncaught exceptions.
    */
  def computation(parallelism: Int, r: UncaughtExceptionReporter = LogExceptionsToStandardErr): Scheduler = {
    val exceptionHandler = new UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable) =
        r.reportFailure(e)
    }

    val pool = new scala.concurrent.forkjoin.ForkJoinPool(
      parallelism,
      ForkJoinPool.defaultForkJoinWorkerThreadFactory,
      exceptionHandler,
      true // asyncMode
    )

    val context = ExecutionContext.fromExecutor(pool, r.reportFailure)
    AsyncScheduler(defaultScheduledExecutor, context, r)
  }

  /** Creates a [[Scheduler]] meant for blocking I/O tasks.
    *
    * Characteristics:
    *
    * - backed by a cached `ThreadPool` executor with 60 seconds of keep-alive
    * - the maximum number of threads is unbounded, as recommended for blocking I/O
    * - uses Scalax's default `ScheduledExecutorService` instance for scheduling
    * - doesn't cooperate with Scala's `BlockContext` because it is unbounded
    *
    * @param name the created threads name prefix, for easy identification.
    *
    * @param daemonic specifies whether the created threads should be daemonic
    *                 (non-daemonic threads are blocking the JVM process on exit).
    *
    * @param r is the [[UncaughtExceptionReporter]] that logs uncaught exceptions.
    */
  def io(name: String = "scalax-io", daemonic: Boolean = true,
    r: UncaughtExceptionReporter = LogExceptionsToStandardErr): Scheduler = {
    val threadFactory = new ThreadFactory {
      private[this] val counter = Atomic(0L)
      def newThread(r: Runnable): Thread = {
        val th = new Thread(r)
        th.setDaemon(daemonic)
        th.setName(name + "-" + counter.getAndIncrement().toString)
        th
      }
    }

    val context = ExecutionContext.fromExecutor(
      Executors.newCachedThreadPool(threadFactory),
      r.reportFailure
    )

    AsyncScheduler(defaultScheduledExecutor, context, r)
  }

  /** Builds a [[Scheduler]] that schedules and executes tasks on its own thread.
    *
    * Characteristics:
    *
    * - backed by a single-threaded `ScheduledExecutorService` that takes care
    *   of both scheduling tasks in the future and of executing tasks
    * - does not cooperate with Scala's `BlockingContext`, so tasks should not
    *   block on the result of other tasks scheduled to run on this same thread
    *
    * @param name is the name of the created thread, for easy identification
    *
    * @param daemonic specifies whether the created thread should be daemonic
    *                 (non-daemonic threads are blocking the JVM process on exit)
    *
    * @param r is the [[UncaughtExceptionReporter]] that logs uncaught exceptions.
    */
  def singleThread(name: String, daemonic: Boolean = true,
    r: UncaughtExceptionReporter = LogExceptionsToStandardErr): Scheduler = {

    val executor =
      Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
        def newThread(r: Runnable) = {
          val th = new Thread(r)
          th.setName(name)
          th.setDaemon(daemonic)
          th
        }
      })

    val context = new ExecutionContext {
      def reportFailure(t: Throwable) = r.reportFailure(t)
      def execute(runnable: Runnable) = executor.execute(runnable)
    }

    AsyncScheduler(executor, context, r)
  }

  /** The default `ScheduledExecutor` instance. */
  private[concurrent] lazy val defaultScheduledExecutor =
    Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
      def newThread(r: Runnable): Thread = {
        val th = new Thread(r)
        th.setDaemon(true)
        th.setName("scalax-scheduler")
        th
      }
    })

  /** The explicit global `Scheduler`. Invoke `global` when you want to provide the global
    * `Scheduler` explicitly.
    *
    * The default `Scheduler` implementation is backed by a work-stealing thread pool, along
    * with a single-threaded `ScheduledExecutionContext` that does the scheduling. By default,
    * the thread pool uses a target number of worker threads equal to the number of
    * [[https://docs.oracle.com/javase/8/docs/api/java/lang/Runtime.html#availableProcessors-- available processors]].
    *
    * @return the global `Scheduler`
    */
  def global: Scheduler = Implicits.global

  object Implicits {
    /** A global [[Scheduler]] instance, provided for convenience, piggy-backing
      * on top of Scala's own `concurrent.ExecutionContext.global`, which is a
      * `ForkJoinPool`.
      *
      * It can be tuned by setting the following JVM system properties:
      *
      * - "scala.concurrent.context.minThreads" an integer specifying the minimum
      *   number of active threads in the pool
      *
      * - "scala.concurrent.context.maxThreads" an integer specifying the maximum
      *   number of active threads in the pool
      *
      * - "scala.concurrent.context.numThreads" can be either an integer,
      *   specifying the parallelism directly or a string with the format "xNUM"
      *   (e.g. "x1.5") specifying the multiplication factor of the number of
      *   available processors (taken with `Runtime.availableProcessors`)
      *
      * The formula for calculating the parallelism in our pool is
      * `min(maxThreads, max(minThreads, numThreads))`.
      *
      * To set a system property from the command line, a JVM parameter must be
      * given to the `java` command as `-Dname=value`. So as an example, to customize
      * this global scheduler, we could start our process like this:
      *
      * <pre>
      *   java -Dscala.concurrent.context.minThreads=2 \
      *        -Dscala.concurrent.context.maxThreads=30 \
      *        -Dscala.concurrent.context.numThreads=x1.5 \
      *        ...
      * </pre>
      *
      * As a note, this being backed by Scala's own global execution context,
      * it is cooperating with Scala's BlockContext, so when operations marked
      * with `scala.concurrent.blocking` are encountered, the thread-pool may
      * decide to add extra threads in the pool. However this is not a thread-pool
      * that is optimal for doing blocking operations, so for example if you want
      * to do a lot of blocking I/O, then use a Scheduler backed by a
      * thread-pool that is more optimal for blocking. See for example
      * [[Scheduler.io]].
      */
    implicit lazy val global: Scheduler =
      AsyncScheduler(
        defaultScheduledExecutor,
        ExecutionContext.Implicits.global,
        UncaughtExceptionReporter.LogExceptionsToStandardErr
      )
  }
}