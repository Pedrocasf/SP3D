package SP3D

import SP3D.Point.numerical
import spinal.core._
import spinal.core.internals.Operator
import spinal.idslplugin.Location

import scala.collection.Seq

object Point {
  type numerical[K<: Data] = Data with Num[K] with MinMaxProvider with DataPrimitives[K] with BitwiseOp[K]
}

case class Point[T <: numerical[T]](val data_type :HardType[T]) extends Bundle with Num[Point[T]] with MinMaxProvider with DataPrimitives[Point[T]] with BitwiseOp[Point[T]] {
  val x = data_type()
  val y = data_type()

  override def tag(q: QFormat): Point[T] = this

  override def +(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x + right.x
    p.y := this.y + right.y
    p
  }

  override def +^(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x +^ right.x
    p.y := this.y +^ right.y
    p
  }

  override def +|(right: Point[T]): Point[T] ={
    val p = new Point[T](data_type)
    p.x := this.x +| right.x
    p.y := this.y +| right.y
    p
  }
  override def -(right: Point[T]): Point[T] ={
    val p = new Point[T](data_type)
    p.x := this.x - right.x
    p.y := this.y - right.y
    p
  }
  override def -^(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x -^ right.x
    p.y := this.y -^ right.y
    p
  }

  override def -|(right: Point[T]): Point[T] ={
    val p = new Point[T](data_type)
    p.x := this.x -| right.x
    p.y := this.y -| right.y
    p
  }
  override def *(right: Point[T]): Point[T] ={
    val p = new Point[T](data_type)
    p.x := this.x * right.x
    p.y := this.y * right.y
    p
  }

  override def /(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x / right.x
    p.y := this.y / right.y
    p
  }
  override def %(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x % right.x
    p.y := this.y % right.y
    p
  }

  override def <(right: Point[T]): Bool = {
    if (this.y == right.y) {
      return this.x < right.x
    }
    this.y < right.y
  }
  override def <=(right: Point[T]): Bool = {
    if (this.y == right.y) {
      return this.x <= right.x
    }
    this.y <= right.y
  }

  override def >(right: Point[T]): Bool = right < this

  override def >=(right: Point[T]): Bool = right <= this

  override def <<(shift: Int): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x << shift
    p.y := this.y << shift
    p
  }

  override def >>(shift: Int): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x >> shift
    p.y := this.y >> shift
    p
  }

  override def sat(m: Int): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x sat m
    p.y := this.y sat m
    p
  }
  override def trim(m: Int): Point[T] ={
    val p = new Point[T](data_type)
    p.x := this.x trim m
    p.y := this.y trim m
    p
  }

  override def floor(n: Int): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x floor n
    p.y := this.y floor n
    p
  }

  override def ceil(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x ceil (n, align)
    p.y := this.y ceil (n, align)
    p
  }

  override def floorToZero(n: Int): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x floorToZero n
    p.y := this.y floorToZero n
    p
  }

  override def ceilToInf(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x ceilToInf (n, align)
    p.y := this.y ceilToInf (n, align)
    p
  }

  override def roundUp(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x roundUp (n, align)
    p.y := this.y roundUp (n, align)
    p
  }

  override def roundDown(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x roundDown (n, align)
    p.y := this.y roundDown (n, align)
    p
  }

  override def roundToZero(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x roundToZero (n, align)
    p.y := this.y roundToZero (n, align)
    p
  }

  override def roundToInf(n: Int, align: Boolean): Point[T] ={
    val p = new Point[T](data_type)
    p.x := this.x roundToInf (n, align)
    p.y := this.y roundToInf (n, align)
    p
  }
  override def roundToEven(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x roundToEven (n, align)
    p.y := this.y roundToEven (n, align)
    p
  }
  override def roundToOdd(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x roundToOdd (n, align)
    p.y := this.y roundToOdd (n, align)
    p
  }

  override def round(n: Int, align: Boolean): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x round (n, align)
    p.y := this.y round (n, align)
    p
  }

  override def minValue: BigInt = x.minValue

  override def maxValue: BigInt = x.maxValue

  override def &(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x & right.x
    p.y := this.y & right.y
    p
}

  override def |(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x | right.x
    p.y := this.y | right.y
    p
  }

  override def ^(right: Point[T]): Point[T] = {
    val p = new Point[T](data_type)
    p.x := this.x ^ right.x
    p.y := this.y ^ right.y
    p
  }

  override def unary_~ : Point[T] =  {
    val p = new Point[T](data_type)
    p.x := ~this.x
    p.y := ~this.y
    p
  }

  override def getZero: this.type = {
    val p = new Point[T](data_type)
    p.x := data_type.craft().getZero
    p.y := data_type.craft().getZero
    p.asInstanceOf[this.type]
  }

  override def asBits: Bits = {
    this.x ## this.y
  }

  override def assignFromBits(bits: Bits): Unit = {
    this.x.assignFromBits(bits)
    this.y.assignFromBits(bits)
  }

  override def assignFromBits(bits: Bits, hi: Int, low: Int): Unit = {
    this.x.assignFromBits(bits, hi, low)
    this.y.assignFromBits(bits, hi, low)
  }

  override def getBitsWidth: Int = x.getBitsWidth*2

  override type RefOwnerType = this.type

  override def isEqualTo(that: Any): spinal.core.Bool = that match {
    case that: Point[T]          => that.x === this.x && that.y === this.y
    case _                    => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  override  def isNotEqualTo(that: Any): spinal.core.Bool = that match {
    case that: Point[T]           => that.x =/= this.x || that.y =/= this.y
    case _                    => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  override def _data: SP3D.Point[T] = this

}

