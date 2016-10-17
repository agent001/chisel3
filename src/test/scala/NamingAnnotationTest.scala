// See LICENSE for license details.

package chiselTests

import chisel3._
import org.scalatest._
import org.scalatest.prop._
import chisel3.testers.BasicTester

import chisel3.internal.naming._

object FunctionMockup {
  def apply() = {
    val context = NamingStack.push_context(false)

    val myA = Namer(UInt(1), "myA")
    val myB = Namer(myA +& UInt(2), "myB")
    val myC = Namer(myB +& UInt(3), "myC")

    NamingStack.pop_return_context(myC +& UInt(4), context)
  }
}

class NamedModule extends BasicTester {
  val context = NamingStack.push_context(true)

  val test = Namer(FunctionMockup(), "test")
  val test2 = Namer(test +& UInt(2), "test2")

  stop()

  NamingStack.pop_context(context)
}

class NamingAnnotationSpec extends ChiselPropSpec {
  property("NamedModule should elaborate") {
    assertTesterPasses { new NamedModule }
  }
}
