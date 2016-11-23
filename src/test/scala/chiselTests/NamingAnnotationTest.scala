// See LICENSE for license details.

package chiselTests

import chisel3._
import chisel3.experimental.chiselName
import org.scalatest._
import org.scalatest.prop._
import chisel3.testers.BasicTester

import scala.collection.mutable.ListBuffer

@chiselName
class NamedModule extends BasicTester {
  val expectedNameMap = ListBuffer[(Data, String)]()

  @chiselName
  def FunctionMockup2(): UInt = {
    val my2A = 1.U
    val my2B = my2A +& 2.U
    val my2C = my2B +& 3.U

    expectedNameMap += ((my2B, "test_myNested_my2B"))

    my2C
  }

  @chiselName
  def FunctionMockup(): UInt = {
    val myNested = FunctionMockup2()
    val myA = 1.U + myNested
    val myB = myA +& 2.U
    val myC = myB +& 3.U

    expectedNameMap += ((myNested, "test_myNested"))
    expectedNameMap += ((myA, "test_myA"))
    expectedNameMap += ((myB, "test_myB"))

    myC +& 4.U
  }

  val test = FunctionMockup()
  val test2 = test +& 2.U

  expectedNameMap += ((test, "test"))
  expectedNameMap += ((test2, "test2"))

  stop()
}

/** Ensure no crash happens if a named function is enclosed in a non-named module
  */
class NonNamedModule extends BasicTester {
  @chiselName
  def NamedFunction(): UInt = {
    val myVal = 1.U + 2.U
    myVal
  }

  val test = NamedFunction()
  stop()
}

/** Ensure no crash happens if a named function is enclosed in a non-named function in a named
  * module.
  */
@chiselName
class NonNamedFunction extends BasicTester {
  @chiselName
  def NamedFunction(): UInt = {
    val myVal = 1.U + 2.U
    myVal
  }

  def NonNamedFunction() : UInt = {
    val myVal = NamedFunction()
    myVal
  }

  val test = NamedFunction()
  stop()
}

/** A simple test that checks the recursive function val naming annotation both compiles and
  * generates the expected names.
  */
class NamingAnnotationSpec extends ChiselPropSpec {
  property("NamedModule should have proper names") {
    var module: NamedModule = null
    assertTesterPasses { module = new NamedModule; module }

    for ((ref, name) <- module.expectedNameMap) {
      assert(ref.instanceName == name)
    }
  }

  property("NonNamedModule should elaborate") {
    assertTesterPasses { new NonNamedModule }
  }

  property("NonNamedFunction should elaborate") {
    assertTesterPasses { new NonNamedFunction }
  }
}
