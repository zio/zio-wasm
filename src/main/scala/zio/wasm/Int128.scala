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
}

object Int128 {
  val zero: Int128   = Int128(0L, 0L)
  val minus1: Int128 = Int128(-1L, -1L)

  val MinValue: Int128 = Int128(Long.MinValue, 0L)

  def fromInt(n: Int): Int128   = Int128(0, n.toLong)
  def fromLong(n: Long): Int128 = Int128(0, n)
}
