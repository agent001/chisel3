// See LICENSE for license details.

package chiselTests

import chisel3._
import org.scalatest._
import org.scalatest.prop._
import chisel3.testers.BasicTester

import chisel3.internal.naming._

object FunctionMockup2 {
  def apply(): UInt = {
    val context = NamingStack.push_context(new FunctionNamingContext)

    val my2A = context.name(UInt(1), "my2A")
    val my2B = context.name(my2A +& UInt(2), "my2B")
    val my2C = context.name(my2B +& UInt(3), "my2C")

    return NamingStack.pop_return_context(my2C, context)
  }
}

object FunctionMockup {
  @dump
  def apply(): (UInt, UInt) = {
    val context = NamingStack.push_context(new FunctionNamingContext)

    val myNested = context.name(FunctionMockup2(), "myNested")
    val myA = context.name(UInt(1) + myNested, "myA")
    val myB = context.name(myA +& UInt(2), "myB")
    val myC = context.name(myB +& UInt(3), "myC")

    return NamingStack.pop_return_context((myC +& UInt(4), UInt(2)), context)
  }
}

@dump
class NamedModule extends BasicTester {
  val context = NamingStack.push_context(new ModuleNamingContext)

  val (test, testx) = context.name(FunctionMockup(), "test")
  val test2 = context.name(test +& UInt(2), "test2")
  val testx2 = context.name(testx +& UInt(2), "testx2")

  stop()

  NamingStack.pop_context(context)
}

class NamingAnnotationSpec extends ChiselPropSpec {
  property("NamedModule should elaborate") {
    assertTesterPasses { new NamedModule }
  }
}
