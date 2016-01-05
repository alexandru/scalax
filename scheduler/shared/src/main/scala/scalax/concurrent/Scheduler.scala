package scalax.concurrent

import java.util.concurrent.TimeUnit
import scalax.concurrent.schedulers.SchedulerCompanion
import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext

/** A Scheduler is an `scala.concurrent.ExecutionContext` that additionally can
  * schedule the execution of units of work to run with a delay or periodically.
  */
@implicitNotFound(
  "Cannot find an implicit Scheduler, either " +
  "import scalax.concurrent.Scheduler.Implicits.global or use a custom one")
trait Scheduler extends ExecutionContext with UncaughtExceptionReporter {
  /** Schedules the given `runnable` for immediate execution. */
  def execute(runnable: Runnable): Unit

  /** Reports that an asynchronous computation failed. */
  def reportFailure(t: Throwable): Unit

  /** Schedules the given `runnable` for immediate execution.
    *
    * @return a [[Cancelable]] that can be used to cancel the scheduled task
    *         in case it hasn't been executed yet.
    */
  def scheduleOnce(r: Runnable): Cancelable

  /** Schedules a task to run in the future, after `initialDelay`.
    *
    * For example the following schedules a message to be printed to
    * standard output after 5 minutes:
    * {{{
    *   val task = scheduler.scheduleOnce(5.minutes, new Runnable {
    *     def run() = println("Hello, world!")
    *   })
    *
    *   // later if you change your mind ...
    *   task.cancel()
    * }}}
    *
    * @param initialDelay is the time to wait until the execution happens
    * @param unit is the time unit used for `initialDelay`
    * @param r is the callback to be executed
    *
    * @return a `Cancelable` that can be used to cancel the created task
    *         before execution.
    */
  def scheduleOnce(initialDelay: Long, unit: TimeUnit, r: Runnable): Cancelable

  /** Schedules for execution a periodic task that is first executed
    * after the given initial delay and subsequently with the given
    * delay between the termination of one execution and the
    * commencement of the next.
    *
    * For example the following schedules a message to be printed to
    * standard output every 10 seconds with an initial delay of 5
    * seconds:
    * {{{
    *   val task = s.scheduleWithFixedDelay(5.seconds, 10.seconds, new Runnable {
    *     def run() = println("Hello, world!")
    *   })
    *
    *   // later if you change your mind ...
    *   task.cancel()
    * }}}
    *
    * @param initialDelay is the time to wait until the first execution happens
    * @param delay is the time to wait between 2 successive executions of the task
    * @param unit is the time unit used for the `initialDelay` and the `delay` parameters
    * @param r is the callback to be executed
    *
    * @return a cancelable that can be used to cancel the execution of
    *         this repeated task at any time.
    */
  def scheduleWithFixedDelay(initialDelay: Long, delay: Long, unit: TimeUnit, r: Runnable): Cancelable

  /** Schedules a periodic task that becomes enabled first after the given
    * initial delay, and subsequently with the given period. Executions will
    * commence after `initialDelay` then `initialDelay + period`, then
    * `initialDelay + 2 * period` and so on.
    *
    * If any execution of the task encounters an exception, subsequent executions
    * are suppressed. Otherwise, the task will only terminate via cancellation or
    * termination of the scheduler. If any execution of this task takes longer
    * than its period, then subsequent executions may start late, but will not
    * concurrently execute.
    *
    * For example the following schedules a message to be printed to standard
    * output approximately every 10 seconds with an initial delay of 5 seconds:
    * {{{
    *   val task = scheduler.scheduleAtFixedRate(5.seconds, 10.seconds , new Runnable {
    *     def run() = println("Hello, world!")
    *   })
    *
    *   // later if you change your mind ...
    *   task.cancel()
    * }}}
    *
    * @param initialDelay is the time to wait until the first execution happens
    * @param period is the time to wait between 2 successive executions of the task
    * @param unit is the time unit used for the `initialDelay` and the `period` parameters
    * @param r is the callback to be executed
    *
    * @return a cancelable that can be used to cancel the execution of
    *         this repeated task at any time.
    */
  def scheduleAtFixedRate(initialDelay: Long, period: Long, unit: TimeUnit, r: Runnable): Cancelable

  /** Returns the current time in milliseconds.  Note that while the
    * unit of time of the return value is a millisecond, the
    * granularity of the value depends on the underlying operating
    * system and may be larger.  For example, many operating systems
    * measure time in units of tens of milliseconds.
    *
    * It's the equivalent of `System.currentTimeMillis()`. When wanting
    * to measure time, do not use `System.currentTimeMillis()`
    * directly, prefer this method instead, because then it can be
    * mocked for testing purposes (see for example
    * [[scalax.concurrent.schedulers.TestScheduler TestScheduler]])
    */
  def currentTimeMillis(): Long
}

object Scheduler extends SchedulerCompanion
