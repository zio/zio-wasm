package zio.wasm.syntax

import zio.{Chunk, ZIO}
import zio.test.*
import zio.wasm.*

object TextSpec extends ZIOSpecDefault {

  override def spec: Spec[Any, Any] =
    suite("Text syntax")(
      test("debug") {
        val text = Text()
        val r    = text.plaininstr.printString(
          Instr.ITruncSatF(IntWidth.I32, FloatWidth.F64, Signedness.Unsigned)
        )
        val v    = text.expr.parseString("(i32.mul) i32.add (local.get 10)")
        println(r)
        println(v)
        assertCompletes
      },
      suite("Integers")(
        test("Unsigned integers") {
          val text = Text()
          check(Gen.long) { value =>
            for {
              str    <- ZIO.fromEither(text.u64.printString(value))
              _      <- ZIO.debug(s"$value <= $str")
              result <- ZIO.fromEither(text.u64.parseString(str))
              _      <- ZIO.debug(s"$str => $result")
            } yield assertTrue(result == value)
          }
        },
        test("Unsigned integers in hexadecimal") {
          val text = Text()
          check(Gen.long) { value =>
            val str = "0x" + value.toHexString
            println(str)
            for {
              result <- ZIO.fromEither(text.u64.parseString(str))
            } yield assertTrue(result == value)
          }
        },
        test("Signed integers") {
          val text = Text()
          check(Gen.long) { value =>
            for {
              str    <- ZIO.fromEither(text.s64.printString(value))
              result <- ZIO.fromEither(text.s64.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("Strings")(
        test("String with escaped chars") {
          val text  = Text()
          val value = s"Hello\tWorld\n\nThis is ${0x1f600.toChar}!!!"
          for {
            str    <- ZIO.fromEither(text.string.printString(value))
            result <- ZIO.fromEither(text.string.parseString(str))
          } yield assertTrue(result == value)
        },
        test("Unicode encoding in strings") {
          val text  = Text()
          val value = """"Hello\tWorld\n\nThis is \u{F600}!!!""""
          for {
            result <- ZIO.fromEither(text.string.parseString(value))
          } yield assertTrue(result == "Hello\tWorld\n\nThis is \uF600!!!")
        },
        test("Roundtrip") {
          val text = Text()
          check(Gen.string) { value =>
            for {
              str    <- ZIO.fromEither(text.string.printString(value))
              result <- ZIO.fromEither(text.string.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("FuncType")(
        test("Roundtrip") {
          val text = Text()
          check(AstGen.funcType) { value =>
            for {
              _      <- ZIO.debug(value.toString)
              str    <- ZIO.fromEither(text.functype.printString(value))
              _      <- ZIO.debug(str)
              result <- ZIO.fromEither(text.functype.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("Limits")(
        test("Roundtrip") {
          val text = Text()
          check(AstGen.limits) { value =>
            for {
              _      <- ZIO.debug(value.toString)
              str    <- ZIO.fromEither(text.limits.printString(value))
              _      <- ZIO.debug(str)
              result <- ZIO.fromEither(text.limits.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("GlobalType")(
        test("Roundtrip") {
          val text = Text()
          check(AstGen.globalType) { value =>
            for {
              _      <- ZIO.debug(value.toString)
              str    <- ZIO.fromEither(text.globaltype.printString(value))
              _      <- ZIO.debug(str)
              result <- ZIO.fromEither(text.globaltype.parseString(str))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("expr")(
        test("Example 1") {
          val text   = Text()
          val input1 = """(local.get 10) (i32.const 2) i32.add (i32.const 3) i32.mul"""
          val input2 = """(i32.mul (i32.add (local.get 10) (i32.const 2)) (i32.const 3))"""

          for {
            result1 <- ZIO.fromEither(text.expr.parseString(input1)).exit
            result2 <- ZIO.fromEither(text.expr.parseString(input2)).exit
            _       <- ZIO.debug("result1: " + result1)
            _       <- ZIO.debug("result2: " + result2)
          } yield assertTrue(result1 == result2)
        }
      )
    )
}
