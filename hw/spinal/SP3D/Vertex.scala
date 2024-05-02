package SP3D

import SP3D.Point.numerical
import spinal.core._

case class Vertex[T <: numerical[T]](data_type :HardType[T]) extends Bundle with Num[Vertex[T]] with MinMaxProvider with DataPrimitives[Vertex[T]] with BitwiseOp[Vertex[T]]{
  val pos: Point[T] = Point[T](data_type)
  val texel: Point[T] = Point[T](data_type)
  def x = pos.x
  def y = pos.y
  def u = texel.x
  def v = texel.y

  override def tag(q: QFormat): Vertex[T] = this

  override def +(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos + right.pos
    p.texel := this.texel + right.texel
    p
  }

  override def +^(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos +^ right.pos
    p.texel := this.texel +^ right.texel
    p
  }

  override def +|(right: Vertex[T]): Vertex[T] ={
    val p = new Vertex[T](data_type)
    p.pos := this.pos +| right.pos
    p.texel := this.texel +| right.texel
    p
  }
  override def -(right: Vertex[T]): Vertex[T] ={
    val p = new Vertex[T](data_type)
    p.pos := this.pos - right.pos
    p.texel := this.texel - right.texel
    p
  }
  override def -^(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos -^ right.pos
    p.texel := this.texel -^ right.texel
    p
  }

  override def -|(right: Vertex[T]): Vertex[T] ={
    val p = new Vertex[T](data_type)
    p.pos := this.pos -| right.pos
    p.texel := this.texel -| right.texel
    p
  }
  override def *(right: Vertex[T]): Vertex[T] ={
    val p = new Vertex[T](data_type)
    p.pos := this.pos * right.pos
    p.texel := this.texel * right.texel
    p
  }

  override def /(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos / right.pos
    p.texel := this.texel / right.texel
    p
  }
  override def %(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos % right.pos
    p.texel := this.texel % right.texel
    p
  }

  override def <(right: Vertex[T]): Bool = {
    return this.pos < right.pos
  }
  override def <=(right: Vertex[T]): Bool = {
    return this.pos <= right.pos
  }

  override def >(right: Vertex[T]): Bool = right < this

  override def >=(right: Vertex[T]): Bool = right <= this

  override def <<(shift: Int): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos << shift
    p.texel := this.texel << shift
    p
  }

  override def >>(shift: Int): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos >> shift
    p.texel := this.texel >> shift
    p
  }

  override def sat(m: Int): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos sat m
    p.texel := this.texel sat m
    p
  }
  override def trim(m: Int): Vertex[T] ={
    val p = new Vertex[T](data_type)
    p.pos := this.pos trim m
    p.texel := this.texel trim m
    p
  }

  override def floor(n: Int): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos floor n
    p.texel := this.texel floor n
    p
  }

  override def ceil(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos ceil (n, align)
    p.texel := this.texel ceil (n, align)
    p
  }

  override def floorToZero(n: Int): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos floorToZero n
    p.texel := this.texel floorToZero n
    p
  }

  override def ceilToInf(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos ceilToInf (n, align)
    p.texel := this.texel ceilToInf (n, align)
    p
  }

  override def roundUp(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos roundUp (n, align)
    p.texel := this.texel roundUp (n, align)
    p
  }

  override def roundDown(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos roundDown (n, align)
    p.texel := this.texel roundDown (n, align)
    p
  }

  override def roundToZero(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos roundToZero (n, align)
    p.texel := this.texel roundToZero (n, align)
    p
  }

  override def roundToInf(n: Int, align: Boolean): Vertex[T] ={
    val p = new Vertex[T](data_type)
    p.pos := this.pos roundToInf (n, align)
    p.texel := this.texel roundToInf (n, align)
    p
  }
  override def roundToEven(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos roundToEven (n, align)
    p.texel := this.texel roundToEven (n, align)
    p
  }
  override def roundToOdd(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos roundToOdd (n, align)
    p.texel := this.texel roundToOdd (n, align)
    p
  }

  override def round(n: Int, align: Boolean): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos round (n, align)
    p.texel := this.texel round (n, align)
    p
  }

  override def minValue: BigInt = pos.minValue

  override def maxValue: BigInt = pos.maxValue

  override def &(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos & right.pos
    p.texel := this.texel & right.texel
    p
  }

  override def |(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos | right.pos
    p.texel := this.texel | right.texel
    p
  }

  override def ^(right: Vertex[T]): Vertex[T] = {
    val p = new Vertex[T](data_type)
    p.pos := this.pos ^ right.pos
    p.texel := this.texel ^ right.texel
    p
  }

  override def unary_~ : Vertex[T] =  {
    val p = new Vertex[T](data_type)
    p.pos := ~this.pos
    p.texel := ~this.texel
    p
  }

  override def getZero: this.type = {
    val v = new Vertex[T](data_type)
    v.pos := Point[T](data_type).getZero
    v.texel := Point[T](data_type).getZero
    v.asInstanceOf[this.type]
  }

  override def asBits: Bits = {
    this.pos ## this.texel
  }

  override def assignFromBits(bits: Bits): Unit = {
    this.pos.assignFromBits(bits)
    this.texel.assignFromBits(bits)
  }

  override def assignFromBits(bits: Bits, hi: Int, low: Int): Unit = {
    this.pos.assignFromBits(bits, hi, low)
    this.texel.assignFromBits(bits, hi, low)
  }

  override def getBitsWidth: Int = pos.getBitsWidth + texel.getBitsWidth

  override type RefOwnerType = this.type

  override def isEqualTo(that: Any): spinal.core.Bool = that match {
    case that: Vertex[T]          => that.pos === this.pos && that.texel === this.texel
    case _                    => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  override  def isNotEqualTo(that: Any): spinal.core.Bool = that match {
    case that: Vertex[T]           => that.pos =/= this.pos || that.texel =/= this.texel
    case _                    => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  override def _data: SP3D.Vertex[T] = this
}