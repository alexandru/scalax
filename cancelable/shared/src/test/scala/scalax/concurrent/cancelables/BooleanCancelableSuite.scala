package scalax.concurrent.cancelables

import minitest.SimpleTestSuite

object BooleanCancelableSuite extends SimpleTestSuite {
  test("cancel()") {
    var effect = 0
    val sub = BooleanCancelable(effect += 1)
    assert(effect == 0)
    assert(!sub.isCanceled)

    sub.cancel()
    assert(sub.isCanceled)
    assert(effect == 1)

    sub.cancel()
    assert(sub.isCanceled)
    assert(effect == 1)
  }
}
