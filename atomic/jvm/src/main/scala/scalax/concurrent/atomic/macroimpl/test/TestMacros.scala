package scalax.concurrent.atomic.macroimpl.test

import scalax.concurrent.atomic.macroimpl.FunctionInliner
import scala.language.experimental.macros

object TestMacros {
  import scala.reflect.macros.blackbox._


  def testInlineParamMacro(): Boolean = macro testInlineParamMacroImpl

  def testInlineParamMacroImpl(c: Context)() = {
    import c.universe._
    val inliner = new FunctionInliner[c.type](c)
    val Apply(Function(params, body), args) = q"((x:Int) => x + 1)(10)"
    val actual = inliner.inlineParam(params.head.name,args.head, body)
    val expected = q"10 + 1"
    val result = actual equalsStructure expected
    if(!result)
      println(s"Expected ${expected} but got ${actual}")

    q"$result"
  }

  def testInlineMacro(): Boolean = macro testInlineMacroImpl

  def testInlineMacroImpl(c: Context)() = {
    import c.universe._
    val inliner = new FunctionInliner[c.type](c)

    val result:Boolean = List({
        val actual = inliner(q"((x:Int) => x + 1)(10)")
        val expected = q"10 + 1"
        val r = actual equalsStructure expected
        if(!r)
          println(s"Expected ${expected} but got ${actual}")
        r
      },
      {
        val actual = inliner(q"((x:Int) => x + 1).apply(10)")
        val expected = q"10 + 1"
        val r = actual equalsStructure expected
        if(!r)
          println(s"Expected ${expected} but got ${actual}")
        r
    }).forall(x => x)

    q"$result"
  }

  def testInlineMultipleArgsMacro(): Boolean = macro testInlineMultipleArgsMacroImpl

  def testInlineMultipleArgsMacroImpl(c: Context)() = {
    import c.universe._
    val inliner = new FunctionInliner[c.type](c)

    val inlinedNoApplyFct = inliner(q"((x:Int, y:Int) => {val z = x + 1; y + z})(10, 20)")
    val resultNoApply = inlinedNoApplyFct equalsStructure q"{val z = 10 + 1; 20 + z}"


    val result:Boolean = List({
        val actual = inliner(q"((x:Int, y:Int) => {val z = x + 1; y + z})(10, 20)")
        val expected = q"{val z = 10 + 1; 20 + z}"
        val r = actual equalsStructure expected
        if(!r)
          println(s"Expected ${expected} but got ${actual}")
        r
      },
      {
        val actual = inliner(q"((x:Int) => x + 1).apply(10)")
        val expected = q"10 + 1"
        val r = actual equalsStructure expected
        if(!r)
          println(s"Expected ${expected} but got ${actual}")
        r
      }).forall(x => x)

    q"$result"
  }
}
