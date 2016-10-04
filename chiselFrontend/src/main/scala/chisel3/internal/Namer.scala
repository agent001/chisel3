// See LICENSE for license details.

// This file contains part of the implementation of the naming static annotation system.

package chisel3.internal.naming

// Abstract base class for all implicit names
trait ImplicitName

object ImplicitName {
  implicit def materialize = NoImplicitName
}

// Automagically generated implicit when no enclosing name is available,
// for example when the function is called from a module that didn't have the
// naming annotation applied.
case object NoImplicitName extends ImplicitName

// General trait for when an enclosing name is available.
trait EnclosingName extends ImplicitName {
  def name: String
}

// Concrete class for enclosing names. Allows implicit "overriding", where a
// function takes an implicit EnclosingName but provides a more specific
// ValName to further function calls that expect an implicit enclosing name.
case class ValName (val name: String) extends EnclosingName {
}

object Namer {
  /** Suggest a name (that will be propagated to FIRRTL) for an object, then returns the object
    * itself (so this can be inserted transparently anywhere).
    * Is a no-op (but safe) when applied on objects that aren't named, including non-Chisel data
    * types.
    */
  def apply[T](obj: T, name: ImplicitName): T = {
    obj match {
      case nameable: chisel3.internal.HasId => {
        name match {
          case name: EnclosingName => {
            nameable.suggestName(name.name)
            nameable.asInstanceOf[T]
          }
          case _ => obj
        }
      }
      case _ => obj
    }
  }
}
