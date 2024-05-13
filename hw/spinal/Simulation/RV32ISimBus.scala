package Simulation

import RV32I.RV32IBiCycle
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple._

import scala.language.postfixOps

case class RV32ISimBus (ADDR_WIDTH:Int = 25, RDCYCLES_WIDTH:Int = 32, RESET_ADDR:Int = 0x00000000, FILE_SIZE:BigInt, FILE_PATH:String, ROM_SIZE:BigInt = 16 MiB, RAM_SIZE:BigInt = 8 MiB, SPRAM_SIZE:BigInt = 128 KiB) extends Component{
  val io = new Bundle{}
  val cpu = RV32IBiCycle(ADDR_WIDTH,RDCYCLES_WIDTH,RESET_ADDR)
  val flash = SimROM(FILE_SIZE, FILE_PATH)
  val psram = SimRAM(RAM_SIZE)
  val spram = SimRAM(SPRAM_SIZE)
  val busInterconnect = PipelinedMemoryBusInterconnect()
  busInterconnect.addSlave(flash.io.bus, SizeMapping(0,ROM_SIZE))
  busInterconnect.addSlave(psram.io.bus, SizeMapping(ROM_SIZE,RAM_SIZE))
  busInterconnect.addSlave(spram.io.bus, SizeMapping(ROM_SIZE+RAM_SIZE, SPRAM_SIZE))
  busInterconnect.addMaster(cpu.io.bus)
  busInterconnect.addConnection(cpu.io.bus, flash.io.bus)
  busInterconnect.addConnection(cpu.io.bus, psram.io.bus)
  busInterconnect.addConnection(cpu.io.bus, spram.io.bus)
  busInterconnect.perfConfig()
}

