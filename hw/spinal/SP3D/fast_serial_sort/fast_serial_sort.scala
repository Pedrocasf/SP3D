package SP3D.fast_serial_sort

import SP3D.Point.numerical
import SP3D.{Config, Vertex}
import spinal.core._
import spinal.core.sim.SimDataPimper

case class fast_serial_sort[T <: numerical[T]](data_type:HardType[T], SZ:Int = 3) extends Component{
  assert(SZ >= 2)
  val io = new Bundle{
    val enable = in Bool()
    val write = in Bool()
    val unsorted_data = in(data_type())
    val sorted_data = out(Vec(data_type(), SZ))
  }
  val cell_data_is_pushed = Bits(SZ-1 bits)
  val cell_state = Vec(CellState(), SZ)
  val cell_data = Vec(data_type(), SZ)
  val cell_array = Seq.fill(SZ){sorting_cell(data_type())}
  for(i <- 0 until SZ){
    if(i == 0){
      cell_array(i).io.enable := io.enable
      cell_array(i).io.prev_cell_data_pushed := False
      cell_array(i).io.prev_cell_state := CellState.OCCUPIED
      cell_array(i).io.shift_up := ~io.write
      cell_array(i).io.prev_cell_data := data_type.craft().getZero
      cell_array(i).io.new_data := io.unsorted_data
      cell_array(i).io.next_cell_data := cell_data(i+1)
      cell_data_is_pushed(i) := cell_array(i).io.cell_data_is_pushed
      cell_state(i) := cell_array(i).io.cell_state
      cell_data(i) := cell_array(i).io.cell_data
    }else if(i == SZ-1){
      cell_array(SZ-1).io.enable := io.enable
      cell_array(SZ-1).io.prev_cell_data_pushed := cell_data_is_pushed(i-1)
      cell_array(SZ-1).io.prev_cell_state := cell_state(i-1)
      cell_array(SZ-1).io.shift_up := ~io.write
      cell_array(SZ-1).io.prev_cell_data := cell_data(i-1)
      cell_array(SZ-1).io.new_data := io.unsorted_data
      cell_array(SZ-1).io.next_cell_data := data_type.craft().getZero
      val _ = cell_array(SZ-1).io.cell_data_is_pushed
      cell_state(i) := cell_array(SZ-1).io.cell_state
      cell_data(i) := cell_array(SZ-1).io.cell_data
    }else{
      cell_array(i).io.enable := io.enable
      cell_array(i).io.prev_cell_data_pushed := cell_data_is_pushed(i-1)
      cell_array(i).io.prev_cell_state := cell_state(i-1)
      cell_array(i).io.shift_up := ~io.write
      cell_array(i).io.prev_cell_data := cell_data(i-1)
      cell_array(i).io.new_data := io.unsorted_data
      cell_array(i).io.next_cell_data := cell_data(i+1)
      cell_data_is_pushed(i) := cell_array(i).io.cell_data_is_pushed
      cell_state(i) := cell_array(i).io.cell_state
      cell_data(i) := cell_array(i).io.cell_data
    }
  }
  io.sorted_data <> cell_data
}
object fast_serial_sort extends App{
  Config.spinal.generateVerilog(fast_serial_sort(Vertex(UInt(8 bits)))).printPruned()
}