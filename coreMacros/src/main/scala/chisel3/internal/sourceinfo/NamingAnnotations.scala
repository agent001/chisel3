// See LICENSE for license details.

// Transform implementations for name-propagation related annotations.

package chisel3.internal.naming

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

object NamingTransforms {
  /** Passthrough transform that prints the annottee for debugging purposes.
    */
  def dump(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    import Flag._

    annottees.foreach(tree => println(show(tree)))
    q"..$annottees"
  }
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class dump extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro NamingTransforms.dump
}
