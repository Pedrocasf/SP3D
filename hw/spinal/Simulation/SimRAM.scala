package Simulation

import SP3D.Config
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.misc.BinTools
import spinal.lib._

case class SimRAM (RAMSize:BigInt) extends Component{
  val io = new Bundle{
    val bus = slave(PipelinedMemoryBus(log2Up(RAMSize>>2),32)).simPublic()
  }
  val memory = Mem(Bits(32 bits), RAMSize>>2)
  memory.randBoot()
  io.bus.cmd.ready := True
  when(io.bus.cmd.fire){
    when(io.bus.cmd.write){
      io.bus.rsp.valid := False
      io.bus.rsp.payload.data := 0
      memory(io.bus.cmd.address) := io.bus.cmd.data
    }otherwise {
      io.bus.rsp.valid := True
      io.bus.rsp.payload.data := memory(io.bus.cmd.address)
    }
  }otherwise{
    io.bus.rsp.valid := False
    io.bus.rsp.payload.data := 0
  }
}
object SimRAM extends App{
  Config.spinal.generateVerilog(SimRAM(8 MiB))
}
