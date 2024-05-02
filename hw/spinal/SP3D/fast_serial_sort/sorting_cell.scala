package SP3D.fast_serial_sort

import SP3D.Point.numerical
import SP3D.Vertex
import spinal.core._
import spinal.core.sim.SimDataPimper
import SP3D.Config
import spinal.lib._
import scala.language.postfixOps

case class sorting_cell[T <: numerical[T]](data_type :HardType[T]) extends Component {
  val io = new Bundle {
    val prev_cell_state = in(CellState())
    val prev_cell_data_pushed = in Bool()
    val shift_up = in Bool()
    val enable = in Bool()
    val prev_cell_data = in(data_type())
    val new_data = in(data_type())
    val next_cell_data = in(data_type())
    val cell_data_is_pushed = out Bool()
    val cell_state = out(Reg(CellState()))
    val cell_data = out(Reg(data_type())) init data_type.craft().getZero
  }
  io.cell_state.init(CellState.EMPTY)
  val new_data_fits = (io.new_data < io.cell_data) || (io.cell_state === CellState.EMPTY)
  io.cell_data_is_pushed <> (new_data_fits & (io.cell_state === CellState.OCCUPIED))
  val priority_vector = Vec(io.prev_cell_state.asBits,io.cell_state.asBits,io.prev_cell_data_pushed,new_data_fits, io.shift_up)
  when(io.enable){
    switch(io.cell_state) {
      is(CellState.EMPTY) {
        io.cell_state := Mux(io.prev_cell_data_pushed, CellState.OCCUPIED, Mux(io.prev_cell_state === CellState.OCCUPIED, CellState.OCCUPIED, CellState.EMPTY))
      }
      is(CellState.OCCUPIED) {
        io.cell_state := CellState.OCCUPIED
      }
    }
  }
  when(io.enable){
    switch(priority_vector.asBits) {
      is(M"0-1--") {
        io.cell_data := io.prev_cell_data
      }
      is(M"0101-") {
        io.cell_data := io.new_data
      }
      is(M"0-001") {
        io.cell_data := io.new_data
      }
      is(M"1----") {
        io.cell_data := io.next_cell_data
      }
      default {
        io.cell_data := io.cell_data
      }
    }
  }otherwise{
    io.cell_data := io.cell_data
  }
}
object sorting_cell extends App{
  Config.spinal.generateVerilog(sorting_cell(SInt(8 bits)))
}