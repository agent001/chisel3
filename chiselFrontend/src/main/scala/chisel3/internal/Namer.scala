// See LICENSE for license details.

// This file contains part of the implementation of the naming static annotation system.

package chisel3.internal.naming

object Namer {
  /** Suggest a name (that will be propagated to FIRRTL) for an object, then returns the object
    * itself (so this can be inserted transparently anywhere).
    * Is a no-op (but safe) when applied on objects that aren't named, including non-Chisel data
    * types.
    */
  def apply[T](obj: T, name: String): T = {
    obj match {
      case other => other
    }
  }
}
