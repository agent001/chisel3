// See LICENSE for license details.

package chiselTests

import chisel3._
import org.scalatest._
import org.scalatest.prop._
import chisel3.testers.BasicTester

import scala.collection.mutable.ListBuffer

@chiselName
class NamedModule extends BasicTester {
  val expectedNameMap = ListBuffer[(Data, String)]()

  @chiselName
  def FunctionMockup2(): UInt = {
    val my2A = UInt(1)
    val my2B = my2A +& UInt(2)
    val my2C = my2B +& UInt(3)

    expectedNameMap += ((my2B, "test_myNested_my2B"))

    my2C
  }

  @chiselName
  def FunctionMockup(): UInt = {
    val myNested = FunctionMockup2()
    val myA = UInt(1) + myNested
    val myB = myA +& UInt(2)
    val myC = myB +& UInt(3)

    expectedNameMap += ((myNested, "test_myNested"))
    expectedNameMap += ((myA, "test_myA"))
    expectedNameMap += ((myB, "test_myB"))

    myC +& UInt(4)
  }

  val test = FunctionMockup()
  val test2 = test +& UInt(2)

  expectedNameMap += ((test, "test"))
  expectedNameMap += ((test2, "test2"))

  stop()
}

/** A simple test that checks the recursive function val naming annotation both compiles and
  * generates the expected names.
  */
class NamingAnnotationSpec extends ChiselPropSpec {
  property("NamedModule should elaborate") {
    var module: NamedModule = null
    assertTesterPasses { module = new NamedModule; module }

    for ((ref, name) <- module.expectedNameMap) {
      assert(ref.instanceName == name)
    }
  }
}
