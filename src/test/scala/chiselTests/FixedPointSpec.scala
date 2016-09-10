// See LICENSE for license details.

package chiselTests

import chisel3._
import chisel3.testers.BasicTester
import org.scalatest._
import org.scalatest.prop._

//scalastyle:off magic.number

class FixedPointSpec extends FlatSpec with Matchers {
  behavior of "fixed point utilities"

  they should "allow conversion between doubles and the bigints needed to represent them" in {
    val initialDouble = 0.125
    val bigInt = FixedPoint.toBigInt(initialDouble, 4)
    val finalDouble = FixedPoint.toDouble(bigInt, 4)

    initialDouble should be(finalDouble)
  }
}

class SBP extends Module {
  val io = new Bundle {
    val in = FixedPoint(INPUT, 6, 2)
    val out = FixedPoint(OUTPUT, 4, 0)
  }
  io.out := io.in.setBinaryPoint(0)
}
class SBPTester extends BasicTester {
  val dut = Module(new SBP)
  dut.io.in := FixedPoint.fromDouble(3.75, binaryPoint = 2)

  assert(dut.io.out === FixedPoint.fromDouble(3.0, binaryPoint = 0))

  stop()
}
class SBPSpec extends ChiselPropSpec {

  property("should allow set binary point") {
//    val c = Driver.elaborate(() => new SBPTester)
//    val e = Driver.emit(c)
//    println(e)
    assertTesterPasses { new SBPTester }
  }
}
