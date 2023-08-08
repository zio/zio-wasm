package zio.wasm.internal

import zio.Chunk
import zio.parser.*
import zio.wasm.syntax.SyntaxError

import scala.reflect.ClassTag

private[wasm] object BinarySyntax {
  val mediaType: String        = "application/wasm"
  val defaultExtension: String = ".wasm"

  type BinarySyntax[A] = Syntax[SyntaxError, Byte, Byte, A]
  type BinaryReader[A] = Parser[SyntaxError, Byte, A]
  type BinaryWriter[A] = Printer[SyntaxError, Byte, A]

  val anyByte  = Syntax.any[Byte]
  val anyBytes = anyByte.*

  def specificByte(value: Byte): Syntax[SyntaxError, Byte, Byte, Byte] =
    anyByte.filter(_ == value, SyntaxError.UnexpectedByte)

  def specificByte_(value: Byte): BinarySyntax[Unit] =
    specificByte(value).unit(value)

  enum Prefix {
    case None
    case Single(byte: Byte)
    case Multiple(bytes: Chunk[Byte])

    def asSyntax: BinarySyntax[Unit] =
      this match {
        case Prefix.None     => Syntax.unit
        case Single(byte)    => specificByte_(byte)
        case Multiple(bytes) => bytes.foldLeft[BinarySyntax[Unit]](Syntax.unit)(_ ~> specificByte_(_))
      }
  }

  object Prefix {
    def apply(byte: Byte): Prefix = Single(byte)

    def apply(byte0: Byte, byte1: Byte, rest: Byte*): Prefix = Multiple(byte0 +: byte1 +: Chunk.fromIterable(rest))
  }

  def casesByPrefix[T, S1 <: T: ClassTag, S2 <: T: ClassTag](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[T, S1 <: T: ClassTag, S2 <: T: ClassTag, S3 <: T: ClassTag](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[T, S1 <: T: ClassTag, S2 <: T: ClassTag, S3 <: T: ClassTag, S4 <: T: ClassTag](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[T, S1 <: T: ClassTag, S2 <: T: ClassTag, S3 <: T: ClassTag, S4 <: T: ClassTag, S5 <: T: ClassTag](
      name: String
  )(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[
      T,
      S1 <: T: ClassTag,
      S2 <: T: ClassTag,
      S3 <: T: ClassTag,
      S4 <: T: ClassTag,
      S5 <: T: ClassTag,
      S6 <: T: ClassTag
  ](
      name: String
  )(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5]),
      case6: => (Prefix, BinarySyntax[S6])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) |
      (case6._1.asSyntax ~> case6._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[
      T,
      S1 <: T: ClassTag,
      S2 <: T: ClassTag,
      S3 <: T: ClassTag,
      S4 <: T: ClassTag,
      S5 <: T: ClassTag,
      S6 <: T: ClassTag,
      S7 <: T: ClassTag
  ](
      name: String
  )(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5]),
      case6: => (Prefix, BinarySyntax[S6]),
      case7: => (Prefix, BinarySyntax[S7])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) |
      (case6._1.asSyntax ~> case6._2).widenWith(SyntaxError.InvalidCase) |
      (case7._1.asSyntax ~> case7._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[
      T,
      S1 <: T: ClassTag,
      S2 <: T: ClassTag,
      S3 <: T: ClassTag,
      S4 <: T: ClassTag,
      S5 <: T: ClassTag,
      S6 <: T: ClassTag,
      S7 <: T: ClassTag,
      S8 <: T: ClassTag
  ](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5]),
      case6: => (Prefix, BinarySyntax[S6]),
      case7: => (Prefix, BinarySyntax[S7]),
      case8: => (Prefix, BinarySyntax[S8])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) |
      (case6._1.asSyntax ~> case6._2).widenWith(SyntaxError.InvalidCase) |
      (case7._1.asSyntax ~> case7._2).widenWith(SyntaxError.InvalidCase) |
      (case8._1.asSyntax ~> case8._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[
      T,
      S1 <: T: ClassTag,
      S2 <: T: ClassTag,
      S3 <: T: ClassTag,
      S4 <: T: ClassTag,
      S5 <: T: ClassTag,
      S6 <: T: ClassTag,
      S7 <: T: ClassTag,
      S8 <: T: ClassTag,
      S9 <: T: ClassTag
  ](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5]),
      case6: => (Prefix, BinarySyntax[S6]),
      case7: => (Prefix, BinarySyntax[S7]),
      case8: => (Prefix, BinarySyntax[S8]),
      case9: => (Prefix, BinarySyntax[S9])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) |
      (case6._1.asSyntax ~> case6._2).widenWith(SyntaxError.InvalidCase) |
      (case7._1.asSyntax ~> case7._2).widenWith(SyntaxError.InvalidCase) |
      (case8._1.asSyntax ~> case8._2).widenWith(SyntaxError.InvalidCase) |
      (case9._1.asSyntax ~> case9._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[
      T,
      S1 <: T: ClassTag,
      S2 <: T: ClassTag,
      S3 <: T: ClassTag,
      S4 <: T: ClassTag,
      S5 <: T: ClassTag,
      S6 <: T: ClassTag,
      S7 <: T: ClassTag,
      S8 <: T: ClassTag,
      S9 <: T: ClassTag,
      S10 <: T: ClassTag
  ](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5]),
      case6: => (Prefix, BinarySyntax[S6]),
      case7: => (Prefix, BinarySyntax[S7]),
      case8: => (Prefix, BinarySyntax[S8]),
      case9: => (Prefix, BinarySyntax[S9]),
      case10: => (Prefix, BinarySyntax[S10])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) |
      (case6._1.asSyntax ~> case6._2).widenWith(SyntaxError.InvalidCase) |
      (case7._1.asSyntax ~> case7._2).widenWith(SyntaxError.InvalidCase) |
      (case8._1.asSyntax ~> case8._2).widenWith(SyntaxError.InvalidCase) |
      (case9._1.asSyntax ~> case9._2).widenWith(SyntaxError.InvalidCase) |
      (case10._1.asSyntax ~> case10._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[
      T,
      S1 <: T: ClassTag,
      S2 <: T: ClassTag,
      S3 <: T: ClassTag,
      S4 <: T: ClassTag,
      S5 <: T: ClassTag,
      S6 <: T: ClassTag,
      S7 <: T: ClassTag,
      S8 <: T: ClassTag,
      S9 <: T: ClassTag,
      S10 <: T: ClassTag,
      S11 <: T: ClassTag
  ](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5]),
      case6: => (Prefix, BinarySyntax[S6]),
      case7: => (Prefix, BinarySyntax[S7]),
      case8: => (Prefix, BinarySyntax[S8]),
      case9: => (Prefix, BinarySyntax[S9]),
      case10: => (Prefix, BinarySyntax[S10]),
      case11: => (Prefix, BinarySyntax[S11])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) |
      (case6._1.asSyntax ~> case6._2).widenWith(SyntaxError.InvalidCase) |
      (case7._1.asSyntax ~> case7._2).widenWith(SyntaxError.InvalidCase) |
      (case8._1.asSyntax ~> case8._2).widenWith(SyntaxError.InvalidCase) |
      (case9._1.asSyntax ~> case9._2).widenWith(SyntaxError.InvalidCase) |
      (case10._1.asSyntax ~> case10._2).widenWith(SyntaxError.InvalidCase) |
      (case11._1.asSyntax ~> case11._2).widenWith(SyntaxError.InvalidCase) ?? name

  def casesByPrefix[
      T,
      S1 <: T: ClassTag,
      S2 <: T: ClassTag,
      S3 <: T: ClassTag,
      S4 <: T: ClassTag,
      S5 <: T: ClassTag,
      S6 <: T: ClassTag,
      S7 <: T: ClassTag,
      S8 <: T: ClassTag,
      S9 <: T: ClassTag,
      S10 <: T: ClassTag,
      S11 <: T: ClassTag,
      S12 <: T: ClassTag
  ](name: String)(
      case1: => (Prefix, BinarySyntax[S1]),
      case2: => (Prefix, BinarySyntax[S2]),
      case3: => (Prefix, BinarySyntax[S3]),
      case4: => (Prefix, BinarySyntax[S4]),
      case5: => (Prefix, BinarySyntax[S5]),
      case6: => (Prefix, BinarySyntax[S6]),
      case7: => (Prefix, BinarySyntax[S7]),
      case8: => (Prefix, BinarySyntax[S8]),
      case9: => (Prefix, BinarySyntax[S9]),
      case10: => (Prefix, BinarySyntax[S10]),
      case11: => (Prefix, BinarySyntax[S11]),
      case12: => (Prefix, BinarySyntax[S12])
  ): BinarySyntax[T] =
    (case1._1.asSyntax ~> case1._2).widenWith(SyntaxError.InvalidCase) |
      (case2._1.asSyntax ~> case2._2).widenWith(SyntaxError.InvalidCase) |
      (case3._1.asSyntax ~> case3._2).widenWith(SyntaxError.InvalidCase) |
      (case4._1.asSyntax ~> case4._2).widenWith(SyntaxError.InvalidCase) |
      (case5._1.asSyntax ~> case5._2).widenWith(SyntaxError.InvalidCase) |
      (case6._1.asSyntax ~> case6._2).widenWith(SyntaxError.InvalidCase) |
      (case7._1.asSyntax ~> case7._2).widenWith(SyntaxError.InvalidCase) |
      (case8._1.asSyntax ~> case8._2).widenWith(SyntaxError.InvalidCase) |
      (case9._1.asSyntax ~> case9._2).widenWith(SyntaxError.InvalidCase) |
      (case10._1.asSyntax ~> case10._2).widenWith(SyntaxError.InvalidCase) |
      (case11._1.asSyntax ~> case11._2).widenWith(SyntaxError.InvalidCase) |
      (case12._1.asSyntax ~> case12._2).widenWith(SyntaxError.InvalidCase) ?? name
}
