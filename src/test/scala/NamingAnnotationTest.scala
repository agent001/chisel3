// See LICENSE for license details.

package chiselTests

import chisel3._
import org.scalatest._
import org.scalatest.prop._
import chisel3.testers.BasicTester

import chisel3.internal.naming._

object FunctionMockup2 {
  @chiselName
  def apply(): UInt = {
    val my2A = UInt(1)
    val my2B = my2A +& UInt(2)
    val my2C = my2B +& UInt(3)
    my2C
  }
}

object FunctionMockup {
  @chiselName
  def apply(): UInt = {
    val myNested = FunctionMockup2()
    val myA = UInt(1) + myNested
    val myB = myA +& UInt(2)
    val myC = myB +& UInt(3)

    myC +& UInt(4)
  }
}

@chiselName
class NamedModule extends BasicTester {
  val test = FunctionMockup()
  val test2 = test +& UInt(2)

  stop()
}

class NamingAnnotationSpec extends ChiselPropSpec {
  property("NamedModule should elaborate") {
    assertTesterPasses { new NamedModule }
  }
}
