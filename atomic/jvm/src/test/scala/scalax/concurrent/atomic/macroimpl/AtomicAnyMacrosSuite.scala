package scalax.concurrent.atomic.macroimpl

import scalax.concurrent.atomic.AtomicAny
import minitest.SimpleTestSuite


object AtomicAnyMacrosSuite extends SimpleTestSuite {
  test("should use the macro transformAndExtract") {
    val r = AtomicAny(0)
    val result = r.transformAndExtractMacro(x => Some(x + 1) -> (x + 1))

    assert(result.contains(1))
    assert(r.get == 1)

    assert(r.transformAndExtractMacro(x => x + 1 -> (x + 1)) == 2)
  }
}
