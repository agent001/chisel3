// See LICENSE for license details.

// Transform implementations for name-propagation related annotations.
//
// Helpful references:
// http://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html#definitions
//   for quasiquote structures of various Scala structures
// http://jsuereth.com/scala/2009/02/05/leveraging-annotations-in-scala.html
//   use of Transformer
// http://www.scala-lang.org/old/sites/default/files/sids/rytz/Wed,%202010-01-27,%2015:10/annots.pdf
//   general annotations reference

package chisel3.internal.naming

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

object NamingTransforms {
  /** Passthrough transform that prints the annottee for debugging purposes.
    * No guarantees are made on what this annotation does, and it may very well change over time.
    *
    * The print is warning level to make it visually easier to spot, as well as a reminder that
    * this annotation should not make it to production / committed code.
    */
  def dump(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    annottees.foreach(tree => c.warning(c.enclosingPosition, s"Debug dump:\n${show(tree)}"))
    q"..$annottees"
  }

  /** Passthrough transform that prints the annottee as a tree for debugging purposes.
    * No guarantees are made on what this annotation does, and it may very well change over time.
    *
    * The print is warning level to make it visually easier to spot, as well as a reminder that
    * this annotation should not make it to production / committed code.
    */
  def treedump(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    annottees.foreach(tree => c.warning(c.enclosingPosition, s"Debug tree dump:\n${showRaw(tree)}"))
    q"..$annottees"
  }

  /** Applies naming transforms to vals in the annotated module. Does not apply recursively to subclasses.
    *
    * Basically rewrites all instances of
    * val name = expr
    * to
    * val name = Namer(expr, name)
    * where Namer is a passthrough which (mutably) populates the expr's resulting object's name
    * field if it has one.
    */
  def module(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    import Flag._
    var wasNamed: Boolean = false

    val valNameTransform = new Transformer {
      override def transform(tree: Tree) = tree match {
        // Intentionally not prefixed with $mods, since modifiers usually mean the val definition
        // is in a non-transformable location, like as a parameter list.
        // TODO: is this exhaustive / correct in all cases?
        case q"val $tname: $tpt = $expr" => {
          val TermName(tnameStr: String) = tname
          val transformedExpr = super.transform(expr)
          q"val $tname: $tpt = _root_.chisel3.internal.naming.Namer($transformedExpr, $tnameStr)"
        }
        case other => super.transform(other)
      }
    }

    val transformed = annottees.map(_ match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
        val transformedStats = valNameTransform.transformTrees(stats)
        wasNamed = true
        q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$transformedStats }"
      }
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
        val transformedStats = valNameTransform.transformTrees(stats)
        wasNamed = true
        q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$transformedStats }"
      }
      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" => {
        // Don't fail noisly when a companion object is passed in with the actual class def
        q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }"
      }
      case other => c.abort(c.enclosingPosition, s"@module annotion may only be used on classes and traits, got ${showCode(other)}")
    })

    if (!wasNamed) {
      // Double check that something was actually transformed
      c.abort(c.enclosingPosition, s"@module annotation did not match a valid tree, got ${annottees.foreach(tree => showCode(tree).mkString(" "))}")
    }

    q"..$transformed"
  }
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class dump extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro NamingTransforms.dump
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class treedump extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro NamingTransforms.treedump
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class module extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro NamingTransforms.module
}
