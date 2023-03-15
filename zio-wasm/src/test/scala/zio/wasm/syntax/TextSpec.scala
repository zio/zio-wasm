package zio.wasm.syntax

import zio.{Chunk, ZIO}
import zio.test.*
import zio.wasm.*

object TextSpec extends ZIOSpecDefault {

  override def spec: Spec[Any, Any] =
    suite("Text syntax")(
      test("debug") {
        given ctx: Text.IdentifierContext = Text.IdentifierContext.empty
        val r = Text.plaininstr.printString(
          Instr.ITruncSatF(IntWidth.I32, FloatWidth.F64, Signedness.Unsigned)
        )
        val v = Text.plaininstr.parseString("memory.init 0")
        println(r)
        println(v)
        assertCompletes
      },
      suite("Integers")(
        test("Unsigned integers")(
          check(Gen.long) { value =>
            for {
              str    <- ZIO.fromEither(Text.u64.printString(value))
              _      <- ZIO.debug(s"$value <= $str")
              result <- ZIO.fromEither(Text.u64.parseString(str))
              _      <- ZIO.debug(s"$str => $result")
            } yield assertTrue(result == value)
          }
        ),
        test("Unsigned integers in hexadecimal")(
          check(Gen.long) { value =>
            val str = "0x" + value.toHexString
            println(str)
            for {
              result <- ZIO.fromEither(Text.u64.parseString(str))
            } yield assertTrue(result == value)
          }
        ),
        test("Signed integers")(
          check(Gen.long) { value =>
            for {
              str    <- ZIO.fromEither(Text.s64.printString(value))
              result <- ZIO.fromEither(Text.s64.parseString(str))
            } yield assertTrue(result == value)
          }
        )
      ),
      suite("Strings")(
        test("String with escaped chars") {
          val value = s"Hello\tWorld\n\nThis is ${0x1f600.toChar}!!!"
          for {
            str    <- ZIO.fromEither(Text.string.printString(value))
            result <- ZIO.fromEither(Text.string.parseString(str))
          } yield assertTrue(result == value)
        },
        test("Unicode encoding in strings") {
          val value = """"Hello\tWorld\n\nThis is \u{F600}!!!""""
          for {
            result <- ZIO.fromEither(Text.string.parseString(value))
          } yield assertTrue(result == "Hello\tWorld\n\nThis is \uF600!!!")
        },
        test("Roundtrip") {
          check(Gen.string) { value =>
            for {
              str    <- ZIO.fromEither(Text.string.printString(value))
              result <- ZIO.fromEither(Text.string.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("FuncType")(
        test("Roundtrip") {
          check(AstGen.funcType) { value =>
            for {
              _      <- ZIO.debug(value.toString)
              str    <- ZIO.fromEither(Text.functype.printString(value))
              _      <- ZIO.debug(str)
              result <- ZIO.fromEither(Text.functype.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("Limits")(
        test("Roundtrip") {
          check(AstGen.limits) { value =>
            for {
              _      <- ZIO.debug(value.toString)
              str    <- ZIO.fromEither(Text.limits.printString(value))
              _      <- ZIO.debug(str)
              result <- ZIO.fromEither(Text.limits.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("GlobalType")(
        test("Roundtrip") {
          check(AstGen.globalType) { value =>
            for {
              _      <- ZIO.debug(value.toString)
              str    <- ZIO.fromEither(Text.globaltype.printString(value))
              _      <- ZIO.debug(str)
              result <- ZIO.fromEither(Text.globaltype.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      )
    )
}
