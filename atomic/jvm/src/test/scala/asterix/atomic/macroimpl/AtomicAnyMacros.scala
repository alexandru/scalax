package asterix.atomic.macroimpl

import asterix.atomic.AtomicAny
import minitest.SimpleTestSuite


object AtomicAnyMacros extends SimpleTestSuite {
  test("should use the macro transformAndExtract") {
    val r = AtomicAny(0)
    val result = r.transformAndExtractMacro(x => Some(x + 1) -> (x + 1))

    assert(result.exists(_ == 1))
    assert(r.get == 1)

    assert(r.transformAndExtractMacro(x => x + 1 -> (x + 1)) == 2)
  }
}
