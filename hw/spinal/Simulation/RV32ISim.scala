package Simulation
import SP3D.Config
import spinal.core.sim._
object RV32ISim extends App {
  Config.sim.withIVerilog.compile(RV32ISimBus(FILE_SIZE = 676, FILE_PATH="hello_rv_world.bin")).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)
    dut.clockDomain.resetSim()
    dut.clockDomain.waitRisingEdge()
    val pc = dut.cpu.pc.toLong
    val instr = dut.cpu.instr.toLong
    print("pc: ", pc.toHexString, '\n')
    print("instr: ", instr.toHexString, '\n')
    while(!dut.cpu.isStop.toBoolean){
      dut.clockDomain.waitRisingEdge()
      val pc = dut.cpu.pc.toLong
      val instr = dut.cpu.instr.toLong
      print("pc: ", pc.toHexString, '\n')
      print("instr: ", instr.toHexString, '\n')
      val addr = dut.cpu.io.bus.cmd.address.toLong
      print("addr: ", addr.toHexString, "\n")

    }
  }
}
