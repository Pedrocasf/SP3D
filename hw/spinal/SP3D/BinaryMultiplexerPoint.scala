package SP3D

import spinal.core.BitVector
import spinal.core.internals.{BinaryMultiplexerWidthable, BitVectorLiteral, BitsLiteral, Expression, InputNormalize, MultiplexerWidthable, Resize, ResizeBits, TypeBits}
object TypePoint
class BinaryMultiplexerPoint extends BinaryMultiplexerWidthable {
  override def getTypeObject: Any = Point
  override def opName: String =  "Bool ? Point | Point"
  override def normalizeInputs: Unit = {
    val targetWidth = getWidth
    whenTrue  = InputNormalize.resizedOrUnfixedLit(whenTrue, targetWidth, new ResizePoint, this, this)
    whenFalse = InputNormalize.resizedOrUnfixedLit(whenFalse, targetWidth, new ResizePoint, this, this)
  }
}
object PointLiteral {

  def apply(value: BigInt, poisonMask: BigInt, specifiedBitCount: Int): BitsLiteral = {
    val valueBitCount  = value.bitLength
    val poisonBitCount = if(poisonMask != null) poisonMask.bitLength else 0
    val minimalWidth   = Math.max(poisonBitCount,valueBitCount)
    var bitCount       = specifiedBitCount

    if (value < 0) throw new Exception("literal value is negative and cannot be represented")

    if (bitCount != -1) {
      if (minimalWidth > bitCount) throw new Exception(s"literal 0x${value.toString(16)} can't fit in Bits($specifiedBitCount bits)")
    } else {
      bitCount = minimalWidth
    }

    BitsLiteral(value, poisonMask, bitCount,specifiedBitCount != -1)
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int, on: T) : T = {
    on.assignFrom(apply(value, null, specifiedBitCount))
    on
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int): BitsLiteral = apply(value, null, specifiedBitCount)

  def apply(value: BigInt, poisonMask: BigInt, bitCount: Int, hasSpecifiedBitCount: Boolean) = {
    val ret = new BitsLiteral
    ret.value = value
    ret.poisonMask = poisonMask
    ret.bitCount = bitCount
    ret.hasSpecifiedBitCount = hasSpecifiedBitCount
    ret
  }
}
class PointLiteral extends BitVectorLiteral{
  override def getTypeObject = TypePoint
  override def isSignedKind: Boolean = false
  override def clone(): this.type = PointLiteral(value, poisonMask, bitCount,hasSpecifiedBitCount).asInstanceOf[this.type]
  override def opName: String = "P\"xxx\""
  override def toString = "(P" + super.toString
}
class ResizePoint extends Resize {
  override def getTypeObject = Point
  override def opName: String = s"resize(Point,$size bits)"
  override def getLiteralFactory: (BigInt, Int) => Expression = PointLiteral.apply
}
class MultiplexerPoint extends MultiplexerWidthable {
  override def getTypeObject: Any = TypePoint
  override def opName: String     = s"mux of Point"
}