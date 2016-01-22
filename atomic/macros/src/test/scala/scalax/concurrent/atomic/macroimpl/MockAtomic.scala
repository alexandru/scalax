package scalax.concurrent.atomic.macroimpl

import scala.language.experimental.macros

import minitest.SimpleTestSuite


class MockAtomic[T](v: T) {
  def transformAndExtract[U](cb: (T) => (U, T)): U = macro AtomicMacros.transformAnExtractMacro[T,U]

  def transformAndGet(cb: (T) => T): T = macro AtomicMacros.transformAndGetMacro[T]

  def getAndTransform(cb: (T) => T): T = macro AtomicMacros.getAndTransformMacro[T]

  def transform(cb: (T) => T): Unit = macro AtomicMacros.transformMacro[T]

  def compareAndSet(expect: T, update: T): Boolean = true

  def get:T = v
}

object AtomicMacrosEvalSuite extends SimpleTestSuite {
  test("macros should compile when used in the context where similarly named vals exists")  {
    val a = new MockAtomic(10)
    val keepTrying = 20
    var result = 10000

    assertEquals(a.transformAndExtract(x => (x,x + 1)), 10)
    assertEquals(a.transformAndGet(_ + 1), 11)
    assertEquals(a.getAndTransform(_ + 1), 10)
    a.transform( _ + 1)


    assert(true)
  }

  test("macros should compile when the lambda sent has vals with similar names")  {
    val a = new MockAtomic(10)

    assertEquals(a.transformAndExtract(x => {
      val keepTrying = 20
      var result = 10000
      (x, x + 1)
    }), 10)

    assertEquals(a.transformAndGet(x => {
      val keepTrying = 20
      var result = 10000
      x + 1
    }), 11)

    assertEquals(a.getAndTransform(x => {
      val keepTrying = 20
      var result = 10000
      x + 1
    }), 10)

    a.transform(x => {
      val keepTrying = 20
      var result = 10000
      x + 1
    })

    assert(true)
  }
}
