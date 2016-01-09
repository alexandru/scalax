package scalax.concurrent.atomic.macroimpl

import minitest.SimpleTestSuite

import scalax.concurrent.atomic.macroimpl.test.TestFunctionInlineMacros


object FunctionInlinerSuite extends SimpleTestSuite{

  test("should inline a symbol in a block of code") {
    assert(TestFunctionInlineMacros.testInlineParamMacro())
  }

  test("inline a function") {
    assert(TestFunctionInlineMacros.testInlineParamMacro())
  }

  test("inline a function with 2 params") {
    assert(TestFunctionInlineMacros.testInlineMultipleArgsMacro())
  }
}
