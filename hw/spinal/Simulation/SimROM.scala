package Simulation
import SP3D.Config
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.misc.BinTools

case class SimROM (ROMSize:BigInt, file:String = null) extends Component{
  val io = new Bundle{
    val bus = slave(PipelinedMemoryBus(log2Up(ROMSize>>2),32)).simPublic()
  }
  val memory = Mem(Bits(32 bits), ROMSize>>2)
  if(file != null) {
    BinTools.initRam(memory, file)
  }
  io.bus.cmd.ready := io.bus.cmd.valid
  io.bus.rsp.payload.data := memory.readSync(io.bus.cmd.payload.address,io.bus.cmd.fire && !io.bus.cmd.payload.write)
  io.bus.rsp.valid := io.bus.cmd.fire && !io.bus.cmd.payload.write
  val write = Reg(Bool()) init False.simPublic()
  write := io.bus.cmd.write
}
object SimROM extends App{
  Config.spinal.generateVerilog(SimROM(16 MiB))
}