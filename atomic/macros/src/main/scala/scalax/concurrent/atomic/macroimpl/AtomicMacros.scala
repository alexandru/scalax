package scalax.concurrent.atomic.macroimpl

import scala.reflect.macros.blackbox.Context

object AtomicMacros {

  def transformMacro[T](c: Context)(cb: c.Tree):c.Tree = {
    import c.universe._

    val functionInliner = new FunctionInliner[c.type](c)

    c.untypecheck(
      q"""
         var keepTrying = true

         while (keepTrying) {
          val current = ${c.prefix.tree}.get
          val update = ${functionInliner(q"$cb(current)")}

          if (${c.prefix.tree}.compareAndSet(current, update)) {
               keepTrying = false
             }
         }

       """
    )
  }

  def getAndTransformMacro[T: c.WeakTypeTag](c: Context)(cb: c.Tree):c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val functionInliner = new FunctionInliner[c.type](c)

    c.untypecheck(
      q"""
         var keepTrying = true
         var result: Option[$tpe] = None

         while (keepTrying) {
          val current = ${c.prefix.tree}.get
          val update = ${functionInliner(q"$cb(current)")}

          if (${c.prefix.tree}.compareAndSet(current, update)) {
               keepTrying = false
               result = Some(current)
             }
         }
         result.get
       """
    )
  }

  def transformAndGetMacro[T: c.WeakTypeTag](c: Context)(cb: c.Tree):c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val functionInliner = new FunctionInliner[c.type](c)

    c.untypecheck(
      q"""
         var keepTrying = true
         var result: Option[$tpe] = None

         while (keepTrying) {
          val current = ${c.prefix.tree}.get
          val update = ${functionInliner(q"$cb(current)")}

          if (${c.prefix.tree}.compareAndSet(current, update)) {
               keepTrying = false
               result = Some(update)
             }
         }
         result.get
       """
    )
  }

  def transformAnExtractMacro[T,U : c.WeakTypeTag](c: Context)(cb: c.Tree):c.Tree = {
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
