package zio.wasm

final case class Int128(high: Long, low: Long) {

  def |(other: Int128): Int128 = Int128(high | other.high, low | other.low)
  def |(other: Int): Int128    = this | Int128.fromInt(other)
  def &(other: Int128): Int128 = Int128(high & other.high, low & other.low)
  def &(other: Int): Int128    = this & Int128.fromInt(other)
  def ^(other: Int128): Int128 = Int128(high ^ other.high, low ^ other.low)
  def ^(other: Int): Int128    = this ^ Int128.fromInt(other)
  def unary_~ : Int128         = Int128(~high, ~low)

  def <<(x: Int): Int128 =
    if (x < 64) Int128((high << x) | (low >>> 1 >>> (63 - x)), low << x)
    else Int128(low << (x - 64), 0)

  def >>(x: Int): Int128 =
    if (x < 64) Int128(high >> x, (low >>> x) | (high << 1 << (63 - x)))
    else Int128(high >> 63, high >> (x - 64))

  def >>>(x: Int): Int128 =
    if (x < 64) Int128(high >>> x, (low >>> x) | (high << 1 << (63 - x)))
    else Int128(0, high >>> (x - 64))

  override def toString: String =
    s"<${high.toHexString}, ${low.toHexString}>"

  def toBinaryString: String = {
    val h = high.toBinaryString
    val l = low.toBinaryString
    ("0" * (64 - h.length) + h + "0" * (64 - l.length) + l).grouped(8).mkString(" ")
  }

  def toByte: Byte   = low.toByte
  def toShort: Short = low.toShort
  def toInt: Int     = low.toInt
  def toLong: Long   = low

  def toI8x16: (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte) =
    (
      (high >>> 56).toByte,
      (high >>> 48).toByte,
      (high >>> 40).toByte,
      (high >>> 32).toByte,
      (high >>> 24).toByte,
      (high >>> 16).toByte,
      (high >>> 8).toByte,
      high.toByte,
      (low >>> 56).toByte,
      (low >>> 48).toByte,
      (low >>> 40).toByte,
      (low >>> 32).toByte,
      (low >>> 24).toByte,
      (low >>> 16).toByte,
      (low >>> 8).toByte,
      low.toByte
    )

  def toI16x8: (Short, Short, Short, Short, Short, Short, Short, Short) =
    (
      (high >>> 48).toShort,
      (high >>> 32).toShort,
      (high >>> 16).toShort,
      high.toShort,
      (low >>> 48).toShort,
      (low >>> 32).toShort,
      (low >>> 16).toShort,
      low.toShort
    )

  def toI32x4: (Int, Int, Int, Int) =
    (
      (high >>> 32).toInt,
      high.toInt,
      (low >>> 32).toInt,
      low.toInt
    )

  def toI64x2: (Long, Long) =
    (
      high,
      low
    )

  def toF32x4: (Float, Float, Float, Float) =
    (
      java.lang.Float.intBitsToFloat((high >>> 32).toInt),
      java.lang.Float.intBitsToFloat(high.toInt),
      java.lang.Float.intBitsToFloat((low >>> 32).toInt),
      java.lang.Float.intBitsToFloat(low.toInt)
    )

  def toF64x2: (Double, Double) =
    (
      java.lang.Double.longBitsToDouble(high),
      java.lang.Double.longBitsToDouble(low)
    )
}

object Int128 {
  val zero: Int128   = Int128(0L, 0L)
  val minus1: Int128 = Int128(-1L, -1L)

  val MinValue: Int128 = Int128(Long.MinValue, 0L)

  def fromInt(n: Int): Int128   = Int128(0, n.toLong)
  def fromLong(n: Long): Int128 = Int128(0, n)

  def i8x16(
      b1: Byte,
      b2: Byte,
      b3: Byte,
      b4: Byte,
      b5: Byte,
      b6: Byte,
      b7: Byte,
      b8: Byte,
      b9: Byte,
      b10: Byte,
      b11: Byte,
      b12: Byte,
      b13: Byte,
      b14: Byte,
      b15: Byte,
      b16: Byte
  ): Int128 =
    Int128(
      (b1.toLong << 56) | (b2.toLong << 48) | (b3.toLong << 40) | (b4.toLong << 32) | (b5.toLong << 24) | (b6.toLong << 16) | (b7.toLong << 8) | b8.toLong,
      (b9.toLong << 56) | (b10.toLong << 48) | (b11.toLong << 40) | (b12.toLong << 32) | (b13.toLong << 24) | (b14.toLong << 16) | (b15.toLong << 8) | b16.toLong
    )

  def i16x8(s1: Short, s2: Short, s3: Short, s4: Short, s5: Short, s6: Short, s7: Short, s8: Short): Int128 =
    Int128(
      (s1.toLong << 48) | (s2.toLong << 32) | (s3.toLong << 16) | s4.toLong,
      (s5.toLong << 48) | (s6.toLong << 32) | (s7.toLong << 16) | s8.toLong
    )

  def i32x4(i1: Int, i2: Int, i3: Int, i4: Int): Int128 =
    Int128(
      (i1.toLong << 32) | i2.toLong,
      (i3.toLong << 32) | i4.toLong
    )

  def i64x2(l1: Long, l2: Long): Int128 =
    Int128(l1, l2)

  def f32x4(i1: Float, i2: Float, i3: Float, i4: Float): Int128 =
    Int128(
      (java.lang.Float.floatToIntBits(i1).toLong << 32) | java.lang.Float.floatToIntBits(i2).toLong,
      (java.lang.Float.floatToIntBits(i3).toLong << 32) | java.lang.Float.floatToIntBits(i4).toLong
    )

  def f64x2(l1: Double, l2: Double): Int128 =
    Int128(java.lang.Double.doubleToLongBits(l1), java.lang.Double.doubleToLongBits(l2))
}
