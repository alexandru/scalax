package scalax.concurrent.atomic.macroimpl

import minitest.SimpleTestSuite

import scalax.concurrent.atomic.macroimpl.test.TestMacros


object FunctionInlinerSuite extends SimpleTestSuite{

  test("should inline a symbol in a block of code") {
    assert(TestMacros.testInlineParamMacro())
  }

  test("inline a function") {
    assert(TestMacros.testInlineParamMacro())
  }

  test("inline a function with 2 params") {
    assert(TestMacros.testInlineMultipleArgsMacro())
  }
}
