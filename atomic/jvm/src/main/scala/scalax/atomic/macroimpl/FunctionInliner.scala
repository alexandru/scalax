package scalax.atomic.macroimpl

import scala.reflect.macros.blackbox._

class FunctionInliner[C <: Context](val c: C) {
  import c.universe._

  val ApplyMethod = TermName("apply")

  def inlineParam(paramTermName: TermName, arg: Tree, body: Tree): Tree = new Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case i@Ident(_) if i.name == paramTermName => arg
        
      case _ => super.transform(tree)
    }
  }.transform(body)

  def inline(function: Tree): Tree = new Transformer {
    override def transform(tree: Tree): Tree = tree match {

      case Apply(Function(params,body),args) =>
        params.zip(args).foldLeft(body){ (b,paramArgs) =>
          val (param, arg) = paramArgs
          inlineParam(param.name, arg, b)
        }

      case Apply(Select(Function(params,body), ApplyMethod), args) =>
        params.zip(args).foldLeft(body){ (b,paramArgs) =>
          val (param, arg) = paramArgs
          inlineParam(param.name, arg, b)
        }

      case _ => super.transform(tree)
    }
  }.transform(function)

  def apply(function: Tree):Tree =
    c.untypecheck(
      inline(function)
    )
}