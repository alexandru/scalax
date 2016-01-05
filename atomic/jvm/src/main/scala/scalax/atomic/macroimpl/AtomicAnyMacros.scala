package scalax.atomic.macroimpl

import scalax.concurrent.atomic.AtomicAny
import scala.reflect.macros.blackbox.Context

object AtomicAnyMacros {
  def transformAnExtractMacro[T,U : c.WeakTypeTag](c: Context  { type PrefixType = AtomicAny[T] })(cb: c.Tree):c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[U]
    val functionInliner = new FunctionInliner[c.type](c)

    c.untypecheck(
      q"""
         var keepTrying = true
         var result: Option[$tpe] = None

         while (keepTrying) {
          val current = ${c.prefix.tree}.get
          val (r, update) = ${functionInliner(q"$cb(current)")}

          if (${c.prefix.tree}.compareAndSet(current, update)) {
               keepTrying = false
               result = Some(r)
             }
         }
         result.get
       """
    )
  }
}
