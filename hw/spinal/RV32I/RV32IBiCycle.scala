package RV32I

import SP3D.Config
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._
import spinal.lib.bus.simple._
import spinal.lib.fsm._

import scala.language.postfixOps

case class RV32IBiCycle (ADDR_WIDTH:Int = 25, RDCYCLES_WIDTH:Int = 32, RESET_ADDR:Int = 0x00000000) extends Component{
  val io = new Bundle{
    val bus = master(PipelinedMemoryBus(ADDR_WIDTH, 32))
  }
  val pc = (Reg(UInt(ADDR_WIDTH bits)) init RESET_ADDR).simPublic()
  val instr = (Reg(Bits(32 bits)) init 0).simPublic()
  val rdId:UInt = instr(11 downto 7).asUInt
  val funct3Is:Bits = B(1) << U(instr(14 downto 12))
  val UImm = B(21 bits, default -> instr(31)) ## instr(30 downto 20)
  val SImm = instr(31) ## instr(30 downto 12) ## B"b000000000000"
  val IImm = B(21 bits, default -> instr(31)) ## instr(30 downto 25) ## instr(11 downto 7)
  val BImm = B(20 bits, default -> instr(31)) ## instr(7) ## instr(30 downto 25) ## instr(11 downto 8) ## B"0"
  val JImm = B(21 bits, default -> instr(31)) ## instr(19 downto 12) ## instr(20) ## instr(30 downto 21) ## B"0"
  val isALUreg = instr(6 downto 2) === B"01100"
  val isALUimm = instr(6 downto 2) === B"00100"
  val isBranch = instr(6 downto 2) === B"11000"
  val isJALR   = instr(6 downto 2) === B"11001"
  val isJAL    = instr(6 downto 2) === B"11011"
  val isAUIPC  = instr(6 downto 2) === B"00101"
  val isLUI    = instr(6 downto 2) === B"01101"
  val isLoad   = instr(6 downto 2) === B"00000"
  val isStore  = instr(6 downto 2) === B"01000"
  val isSystem = instr(6 downto 2) === B"11100"
  val isStop   = instr(6 downto 2) === B"00011"
  isStop.simPublic()
  val isALU = isALUimm | isALUreg
  val writeBack = Bool()
  val writeBackData = UInt(32 bits)
  val rs1 = Reg(Bits(32 bits)) init 0
  val rs2 = Reg(Bits(32 bits)) init 0
  val registerFile = Mem(Bits(32 bits), 32) init Seq.fill(32)(B"0")
  registerFile.write(rdId, B(writeBackData), writeBack && rdId =/= 0)
  val aluIn1 = rs1
  val aluIn2 = Mux(isALUreg | isBranch, rs2, IImm)
  val aluPlus = U(aluIn1) + U(aluIn2)
  val aluMinus = U(B"1" ## ~aluIn2) + U(B"0" ## aluIn1) + U"33'h1FFFFFFFF"
  val LT = Mux(aluIn1(31) ^ aluIn2(31), aluIn1(31), aluMinus(32))
  val LTU = aluMinus(32)
  val EQ = aluMinus(31 downto 0) === U"32'h00000000"
  val shifter_in = Mux(funct3Is(1), aluIn1.reversed, aluIn1)
  val shifter = S((instr(30) & aluIn1(31)) ## shifter_in) >> U(aluIn2(4 downto 0))
  val leftShift = shifter.reversed
  val aluOut = Mux(funct3Is(0), Mux(instr(30) & instr(5), B(aluMinus(31 downto 0)), B(aluPlus)), B"32'h00000000") |
    Mux(funct3Is(1), B(leftShift(31 downto 0)), B"32'h00000000" ) |
    Mux(funct3Is(2), B"31'h00000000" ## LT, B"32'h00000000" ) |
    Mux(funct3Is(3), B"31'h00000000" ## LTU, B"32'h00000000" ) |
    Mux(funct3Is(4), B(aluIn1 ^ aluIn2), B"32'h00000000" ) |
    Mux(funct3Is(5), B(shifter(31 downto 0)), B"32'h00000000" ) |
    Mux(funct3Is(6), B(aluIn1 | aluIn2), B"32'h00000000" ) |
    Mux(funct3Is(7), B(aluIn1 & aluIn2), B"32'h00000000" )
  val predicate =
    funct3Is(0) & EQ |
    funct3Is(1) & !EQ |
    funct3Is(4) & LT |
    funct3Is(5) & !LT |
    funct3Is(6) & LTU |
    funct3Is(7) & !LTU
  val jumpToPCplusImm = isJAL | (isBranch & predicate)
  val PCplus4 = pc+4
  val PCplusImm = pc + Mux(instr(3), U(JImm(ADDR_WIDTH-1 downto 0)), Mux(instr(4), U(UImm(ADDR_WIDTH-1 downto 0)), U(BImm(ADDR_WIDTH-1 downto 0))))
  val loadstore_addr = U(rs1(ADDR_WIDTH-1 downto 0)) + Mux(instr(5), U(SImm(ADDR_WIDTH-1 downto 0)), U(IImm(ADDR_WIDTH-1 downto 0)))
  val PC_new = Mux(isJALR, U(aluPlus(ADDR_WIDTH-1 downto 1) ## B"0"),
    Mux(jumpToPCplusImm, PCplusImm, PCplus4))
  val cycles = Reg(UInt(RDCYCLES_WIDTH bits)) init 0
  cycles := cycles+1
  when(isStop){
    cycles := 0
  }
  val needToWait = isLoad | isStore
  val mem_byteAccess = instr(13 downto 12) === B"00"
  val mem_halfwordAccess = instr(13 downto 12) === B"01"
  val LOAD_halfword:Bits = Mux(loadstore_addr(1), io.bus.rsp.data(31 downto 16), io.bus.rsp.data(15 downto 0))
  val LOAD_byte:Bits = Mux(loadstore_addr(0), LOAD_halfword(15 downto 8), LOAD_halfword(7 downto 0))
  val LOAD_sign = !instr(14) & Mux(mem_byteAccess, LOAD_byte(7), LOAD_halfword(15))
  val LOAD_data = Mux(mem_byteAccess, B(24 bits, default -> LOAD_sign) ## LOAD_byte,
    Mux(mem_halfwordAccess, B(16 bits, default -> LOAD_sign) ## LOAD_halfword, io.bus.rsp.data))
  io.bus.cmd.payload.data(7 downto 0) <> rs2(7 downto 0)
  io.bus.cmd.payload.data(15 downto 8) <> Mux(loadstore_addr(0), rs2(7 downto 0), rs2(15 downto 8))
  io.bus.cmd.payload.data(23 downto 16) <> Mux(loadstore_addr(1), rs2(7 downto 0), rs2(23 downto 16))
  io.bus.cmd.payload.data(31 downto 24) <> Mux(loadstore_addr(0), rs2(7 downto 0), Mux(loadstore_addr(1), rs2(15 downto 8), rs2(31 downto 24)))
  val STORE_wmask =
    Mux(mem_byteAccess,
      Mux(loadstore_addr(1),
        Mux(loadstore_addr(0), B"1000", B"0100"),
        Mux(loadstore_addr(0), B"0010", B"0001")),
      Mux(mem_halfwordAccess,
        Mux(loadstore_addr(1), B"1100", B"0011"),
  B"1111"))
  val fsm = new StateMachine {
    val WAIT_INSTR:State with EntryPoint = new State with EntryPoint {
      whenIsActive {
        rs1 := registerFile.readSync(U(io.bus.rsp.payload.data(19 downto 15)), io.bus.rsp.fire)
        rs2 := registerFile.readSync(U(io.bus.rsp.payload.data(24 downto 20)), io.bus.rsp.fire)
        when(io.bus.rsp.fire) {
          instr(31 downto 2) := io.bus.rsp.payload.data(31 downto 2)
          goto(EXECUTE)
        }
      }
    }
    val EXECUTE = new State {
      whenIsActive {
        pc := PC_new
        when(needToWait) {
          goto(WAIT_MEM)
        } otherwise {
          goto(WAIT_INSTR)
        }
      }
    }
    val WAIT_MEM:State = new State {
      whenIsActive {
        when(io.bus.rsp.fire) {
          goto(FETCH_INSTR)
        }
      }
    }
    val FETCH_INSTR:State = new State{
      whenIsActive{
        goto(WAIT_INSTR)
      }
    }
    val wait_instr_state = this.isActive(WAIT_INSTR)
    val execute_state = this.isActive(EXECUTE)
    val wait_mem_state = this.isActive(WAIT_MEM)
    val fetch_instr_state = this.isActive(FETCH_INSTR)
    io.bus.cmd.address \ Mux(wait_instr_state | fetch_instr_state, pc, Mux(execute_state & !isLoad & !isStore, PC_new, loadstore_addr))
    writeBack \ !(isBranch | isStore) & (execute_state | wait_mem_state)
    io.bus.cmd.mask \ Mux(isStore, B(4 bits, default -> (execute_state & isStore)) & STORE_wmask, B"1111")
    io.bus.cmd.write \ (isStore & execute_state)
    io.bus.cmd.valid \ execute_state & !isStore | fetch_instr_state
  }
    writeBackData :=
      Mux(isSystem, cycles, U(0)) |
        Mux(isLUI, U(UImm), U(0)) |
        Mux(isALU, U(aluOut(31 downto 0)), U(0))|
        Mux(isAUIPC, PCplusImm.resize(32 bits), U(0)) |
        Mux(isJALR | isJAL, PCplus4.resize(32 bits), U(0)) |
        Mux(isLoad, U(LOAD_data), U(0))

}
object RV32IBiCycle extends App{
  Config.spinal.generateVerilog(RV32IBiCycle()).printPruned()
}