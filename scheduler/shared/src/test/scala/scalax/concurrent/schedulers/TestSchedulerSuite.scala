package scalax.concurrent.schedulers

import java.util.concurrent.{TimeUnit, TimeoutException}
import minitest.TestSuite
import scalax.concurrent.Scheduler
import scala.concurrent.duration._
import scala.concurrent.Promise
import scala.util.{Success, Try}

object TestSchedulerSuite extends TestSuite[TestScheduler] {
  def setup() = TestScheduler()
  def tearDown(env: TestScheduler): Unit = {
    assert(env.state.get.tasks.isEmpty)
  }

  test("should execute asynchronously") { s =>
    var wasExecuted = false
    s.execute(action { wasExecuted = true })
    assert(!wasExecuted)

    s.tick()
    assert(wasExecuted)
  }

  test("should execute the whole stack") { s =>
    var iterations = 0

    s.execute(action {
      s.execute(action {
        iterations += 1
        s.execute(action {
          iterations += 1
        })
      })

      assert(iterations == 0)
      iterations += 1

      s.execute(action {
        iterations += 1
        s.execute(action {
          iterations += 1
        })
      })
    })

    assert(iterations == 0)

    s.tick()
    assert(iterations == 5)
  }

  test("should schedule stuff in the future") { s =>
    var firstBatch = 0
    var secondBatch = 0

    s.scheduleOnce(10, TimeUnit.SECONDS, action {
      firstBatch += 1
      s.execute(action { firstBatch += 1 })
      s.scheduleOnce(10, TimeUnit.SECONDS, action {
        firstBatch += 1
      })
    })

    s.scheduleOnce(20, TimeUnit.SECONDS, action {
      secondBatch += 1
      s.execute(action { secondBatch += 1 })
      s.scheduleOnce(10, TimeUnit.SECONDS, action {
        secondBatch += 1
      })
    })

    s.tick()
    assert(firstBatch == 0 && secondBatch == 0)

    s.tick(9.seconds)
    assert(firstBatch == 0 && secondBatch == 0)

    s.tick(1.second)
    assert(firstBatch == 2 && secondBatch == 0)

    s.tick(10.seconds)
    assert(firstBatch == 3 && secondBatch == 2)

    s.tick(10.seconds)
    assert(firstBatch == 3 && secondBatch == 3)
  }

  test("should work correctly for ticks spanning several tasks, test 1") { implicit s =>
    val f = delayedResult(50.millis, 300.millis)("hello world")
    assertEquals(f.value, None)

    s.tick(10.seconds)
    assertEquals(f.value, Some(Success("hello world")))
  }

  test("should work correctly for ticks spanning several tasks, test 2") { implicit s =>
    val f = delayedResult(500.millis, 300.millis)("hello world")
    assertEquals(f.value, None)

    s.tick(10.seconds)
    intercept[TimeoutException](f.value.get.get)
  }

  test("should work correctly for ticks spanning several tasks, test 3") { implicit s =>
    val f = delayedResult(50.millis, 300.millis)("hello world")
    assertEquals(f.value, None)

    s.tick(50.seconds)
    s.tick(300.millis)

    assertEquals(f.value, Some(Success("hello world")))
  }

  test("should work correctly for ticks spanning several tasks, test 4") { implicit s =>
    val f = delayedResult(50.millis, 300.millis)("hello world")
    assertEquals(f.value, None)

    s.tick(40.seconds)
    s.tick(10.seconds)
    s.tick(300.millis)

    assertEquals(f.value, Some(Success("hello world")))
  }

  test("complicated scheduling, test 1") { implicit s =>
    var counter = 0

    delayedResult(50.millis, 300.millis) {
      counter += 1

      delayedResult(50.millis, 300.millis) {
        counter += 1

        delayedResult(50.millis, 300.millis) {
          counter += 1

          delayedResult(50.millis, 300.millis) {
            counter += 1

            delayedResult(50.millis, 300.millis) {
              counter += 1
            }
          }
        }
      }
    }

    s.tick(250.millis)

    assert(counter == 5)
    assert(s.state.get.tasks.isEmpty)
  }

  test("complicated scheduling, test 2") { implicit s =>
    var counter = 0

    delayedResult(50.millis, 300.millis) {
      counter += 1

      delayedResult(50.millis, 300.millis) {
        counter += 1

        delayedResult(50.millis, 300.millis) {
          counter += 1

          delayedResult(50.millis, 300.millis) {
            counter += 1

            delayedResult(50.millis, 300.millis) {
              counter += 1
            }
          }
        }
      }
    }

    for (_ <- 0 until 250) s.tick(1.milli)
    assert(counter == 5)
    assert(s.state.get.tasks.isEmpty)
  }

  test("tasks sharing same runsAt should execute randomly") { implicit s =>
    var seq = Seq.empty[Int]
    for (i <- 0 until 1000) s.execute(action { seq = seq :+ i })
    s.tick()

    val expected = 0 until 1000
    assert(seq != expected)
    assertEquals(seq.sum, expected.sum)
  }

  def action(f: => Unit): Runnable =
    new Runnable { def run() = f }

  def delayedResult[T](delay: FiniteDuration, timeout: FiniteDuration)(r: => T)(implicit s: Scheduler) = {
    val f1 = {
      val p = Promise[T]()
      s.scheduleOnce(delay.length, delay.unit, action(p.success(r)))
      p.future
    }

    // catching the exception here, for non-useless stack traces
    val err = Try(throw new TimeoutException)
    val promise = Promise[T]()
    val task = s.scheduleOnce(timeout.length, timeout.unit, action(promise.tryComplete(err)))

    f1.onComplete { result =>
      // canceling task to prevent waisted CPU resources and memory leaks
      // if the task has been executed already, this has no effect
      task.cancel()
      promise.tryComplete(result)
    }

    promise.future
  }
}
