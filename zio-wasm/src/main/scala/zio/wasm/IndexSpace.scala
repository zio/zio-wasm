package zio.wasm

trait IndexSpace {
  type Idx

  def fromInt(idx: Int): Idx
  def toInt(idx: Idx): Int
}
