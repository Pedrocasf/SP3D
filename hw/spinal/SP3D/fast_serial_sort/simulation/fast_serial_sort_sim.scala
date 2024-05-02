package SP3D.fast_serial_sort.simulation

import spinal.core.sim._
import spinal.core._
import SP3D.{Config, Vertex}
import SP3D.fast_serial_sort.fast_serial_sort
object fast_serial_sort_sim  extends App {
  Config.sim.withIVerilog.compile(fast_serial_sort(Vertex(UInt(8 bits)))).doSim { dut =>
    dut.io.write #= true
    val rand = new scala.util.Random
    dut.clockDomain.forkStimulus(period = 10)
    dut.clockDomain.resetSim()
    dut.clockDomain.waitRisingEdge()
    for (i <- 0 to 32) {
      dut.clockDomain.waitRisingEdge()
      dut.io.enable #= true
      val rx = rand.nextInt(256)
      dut.io.unsorted_data.x #= rx
      println("v", i, ".x:", rx)
      val ry = rand.nextInt(256)
      dut.io.unsorted_data.y #= ry
      println("v", i, ".y:", ry)
      if(i%3 == 0){
        println("v0.x:", dut.io.sorted_data(0).x.toInt)
        println("v0.y:", dut.io.sorted_data(0).y.toInt)
        println("v1.x:", dut.io.sorted_data(1).x.toInt)
        println("v1.y:", dut.io.sorted_data(1).y.toInt)
        println("v2.x:", dut.io.sorted_data(2).x.toInt)
        println("v2.y:", dut.io.sorted_data(2).y.toInt)
        assert(dut.io.sorted_data(0).y.toInt <= dut.io.sorted_data(1).y.toInt, "wrong order of v0.y and v1.y")
        assert(dut.io.sorted_data(1).y.toInt <= dut.io.sorted_data(2).y.toInt, "wrong order of v1.y and v2.y")
      }
      dut.clockDomain.waitRisingEdge()
      dut.io.enable #= false
    }
  }
}