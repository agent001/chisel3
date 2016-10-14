// See LICENSE for license details.

// This file contains part of the implementation of the naming static annotation system.

package chisel3.internal.naming

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

object Namer {
  /** Suggest a name (that will be propagated to FIRRTL) for an object, then returns the object
    * itself (so this can be inserted transparently anywhere).
    * Is a no-op (but safe) when applied on objects that aren't named, including non-Chisel data
    * types.
    */
  def apply[T](obj: T, name: String): T = {
    obj match {
      case nameable: chisel3.internal.HasId => {
        nameable.suggestName(name)
        nameable.asInstanceOf[T]
      }
      case _ => obj
    }
  }
}

/** Recursive Function Namer
  *
  * Summary:
  *
  * In every function, creates a NamingContext object, which associates all vals with a string name
  * suffix, for example:
  *   val name = SomeStatement()
  * produces the entry in items:
  *   {ref of SomeStatement(), "name"}
  *
  * Like the normal namer, all:
  *   val name = SomeStatement()
  * statements are transformed into:
  *   val name = Namer(SomeStatement(), "name", context)
  *
  * The context is created from a global dynamic context stack at the beginning of each function.
  * At the end of each function call, the completed context is added to its parent context and
  * associated with the return value (whose name at an enclosing function call will form the prefix
  * for all named objects).
  *
  * During each Namer(obj, obj_name, context) call, Namer will check the context's descendants list
  * for an entry (desc_obj, desc_ctx) where desc_obj is reference-equal to obj. If there is one, it
  * will append obj_name as a prefix to all of desc_ctx's items, then add those items to its own.
  *
  * At a top-level (Module-level) Namer call, objects will be named directly, instead of being
  * added to a list of items (because there is no more prefixing that is dependent on an enclosing
  * scope).
  *
  * For functions, returns are wrapped in a function that:
  * - pops the current function's entry from the global dynamic context stack; this is
  *   assert-checked to catch edge cases where the stack might have been mismanaged
  * - adds the returned object as a descendent of the enclosing context; this allows the enclosing
  *   function to attach the name of the returned object as a prefix
  */

class NamingContext(val parent: Option[NamingContext]) {
  val descendants = ListBuffer[NamingContext]()
  val items = ListBuffer[(chisel3.internal.HasId, String)]()  // tuple of nameable and suffix

  /** Adds a NamingContext object as a descendant - where its contained objects will have
    *
    */
  def add_descendant(descendant: NamingContext) {
    descendants += { return; descendant }
  }
}

/** Global, singleton
  *
  * Do not call manually. This should only be inserted by macro transforms.
  */
object NamingStack {
  val naming_stack = Stack[NamingContext]()

  /** Creates a new naming context, where all items in the context will have their names prefixed
    * with some yet-to-be-determined prefix from object names in an enclosing scope.
    */
  def push_context(): NamingContext = {
    // TODO: restyle
    val new_context = if (naming_stack.isEmpty) {
      new NamingContext(None)
    } else {
      new NamingContext(Some(naming_stack.top))
    }
    naming_stack.push(new_context)
    new_context
  }

  /** Called at the end of a function, popping the current naming context.
    * Every instance of push_context() must have a matching pop_context().
    *
    * Will assert out if the context being popped isn't the topmost on the stack.
    */
  def pop_context(until: NamingContext) {
    assert(naming_stack.top == until)
    naming_stack.pop()
  }
}

class MyFunctionMockup {
  def callee() {
    val context = NamingStack.push_context()

    NamingStack.pop_context(context)
  }
}
