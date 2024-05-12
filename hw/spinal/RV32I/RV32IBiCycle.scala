package RV32I

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple._

import scala.language.postfixOps

case class RV32IBiCycle (ADDR_WIDTH:Int = 25, RDCYCLES_WIDTH:Int = 32, RESET_ADDR:Int = 0x00000000) extends Component{
  val io = new Bundle{
    val bus = master(PipelinedMemoryBus(ADDR_WIDTH, 32))
  }
  val unknownInstr = Reg(Bool()) init False
  val pc = Reg(UInt(ADDR_WIDTH bits)) init RESET_ADDR
  val instr = Reg(Bits(32 bits)) init 0

}
