package zio.wasm.syntax

import zio.*
import zio.test.*
import zio.wasm.*

object BinarySpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Any] =
    suite("Binary syntax")(
      suite("LEB128")(
        test("Unsigned LEB128 encoding") {
          for {
            b1 <- ZIO.fromEither(Binary.u32.print(0))
            b2 <- ZIO.fromEither(Binary.u32.print(1))
            b3 <- ZIO.fromEither(Binary.u32.print(127))
            b4 <- ZIO.fromEither(Binary.u32.print(16256))
            b5 <- ZIO.fromEither(Binary.u32.print(0x3b4))
            b6 <- ZIO.fromEither(Binary.u32.print(0x40c))
            b7 <- ZIO.fromEither(Binary.u32.print(0xffffffff))
          } yield assertTrue(
            b1 == Chunk[Byte](0),
            b2 == Chunk[Byte](1),
            b3 == Chunk[Byte](0x7f),
            b4 == Chunk[Byte](0x80.toByte, 0x7f),
            b5 == Chunk[Byte](0xb4.toByte, 0x07),
            b6 == Chunk[Byte](0x8c.toByte, 0x08),
            b7 == Chunk[Byte](0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xf)
          )
        },
        test("Unsigned LEB128 decoding") {
          for {
            r1 <- ZIO.fromEither(Binary.u32.parseChunk(Chunk[Byte](0)))
            r2 <- ZIO.fromEither(Binary.u32.parseChunk(Chunk[Byte](1)))
            r3 <- ZIO.fromEither(Binary.u32.parseChunk(Chunk[Byte](0x7f)))
            r4 <- ZIO.fromEither(Binary.u32.parseChunk(Chunk[Byte](0x80.toByte, 0x7f)))
            r5 <- ZIO.fromEither(Binary.u32.parseChunk(Chunk[Byte](0xb4.toByte, 0x07)))
            r6 <- ZIO.fromEither(Binary.u32.parseChunk(Chunk[Byte](0x8c.toByte, 0x08)))
            r7 <-
              ZIO.fromEither(
                Binary.u32.parseChunk(Chunk[Byte](0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xf))
              )
          } yield assertTrue(
            r1 == 0,
            r2 == 1,
            r3 == 127,
            r4 == 16256,
            r5 == 0x3b4,
            r6 == 0x40c,
            r7 == 0xffffffff
          )
        },
        test("Unsigned 32bit LEB128 roundtrip") {
          check(Gen.int) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.u32.print(value))
              result <- ZIO.fromEither(Binary.u32.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        },
        test("Signed 32bit LEB128 roundtrip") {
          check(Gen.int) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.i32.print(value))
              result <- ZIO.fromEither(Binary.i32.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        },
        test("Unsigned 64bit LEB128 roundtrip") {
          check(Gen.long) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.u64.print(value))
              result <- ZIO.fromEither(Binary.u64.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        },
        test("Signed 64bit LEB128 roundtrip") {
          check(Gen.long) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.i64.print(value))
              result <- ZIO.fromEither(Binary.i64.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        },
        test("Unsigned 128bit LEB128 roundtrip") {
          check(AstGen.int128) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.u128.print(value))
              result <- ZIO.fromEither(Binary.u128.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        },
        test("Signed 128bit LEB128 roundtrip") {
          check(AstGen.int128) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.i128.print(value))
              result <- ZIO.fromEither(Binary.i128.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("Vectors")(
        test("Vector of integers roundtrip") {
          check(Gen.chunkOf(Gen.int)) { vec =>
            for {
              bytes  <- ZIO.fromEither(Binary.vec(Binary.u32).print(vec))
              result <- ZIO.fromEither(Binary.vec(Binary.u32).parseChunk(bytes))
            } yield assertTrue(result == vec)
          }
        }
      ),
      suite("Name")(
        test("Name roundtrip") {
          check(Gen.string.map(Name.fromString)) { name =>
            for {
              bytes  <- ZIO.fromEither(Binary.name.print(name))
              result <- ZIO.fromEither(Binary.name.parseChunk(bytes))
            } yield assertTrue(result == name)
          }
        }
      ),
      suite("ValType")(
        test("ValType roundtrip") {
          check(AstGen.valType) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.valType.print(value))
              result <- ZIO.fromEither(Binary.valType.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("FuncType")(
        test("FuncType roundtrip") {
          check(AstGen.funcType) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.funcType.print(value))
              result <- ZIO.fromEither(Binary.funcType.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("Limits")(
        test("Limits roundtrip") {
          check(AstGen.limits) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.limits.print(value))
              result <- ZIO.fromEither(Binary.limits.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("TableType")(
        test("TableType roundtrip") {
          check(AstGen.tableType) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.tableType.print(value))
              result <- ZIO.fromEither(Binary.tableType.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("GlobalType")(
        test("GlobalType roundtrip") {
          check(AstGen.globalType) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.globalType.print(value))
              result <- ZIO.fromEither(Binary.globalType.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("BlockType")(
        test("BlockType roundtrip") {
          check(AstGen.blockType) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.blockType.print(value))
              result <- ZIO.fromEither(Binary.blockType.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        },
        test("BlockType sequence roundtrip") {
          check(Gen.chunkOf(AstGen.blockType)) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.blockType.repeat0.print(value))
              result <- ZIO.fromEither(Binary.blockType.repeat0.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("MemArg")(
        test("MemArg roundtrip") {
          check(AstGen.memArg) { value =>
            for {
              bytes  <- ZIO.fromEither(Binary.memArg.print(value))
              result <- ZIO.fromEither(Binary.memArg.parseChunk(bytes))
            } yield assertTrue(result == value)
          }
        }
      ),
      suite("expr")(
        test("Expr roundtrip 1") {
          val value = Expr(Chunk(Instr.I32Const(1), Instr.I32Const(2), Instr.IAdd(IntWidth.I32)))
          for {
            bytes  <- ZIO.fromEither(Binary.expr.print(value))
            result <- ZIO.fromEither(Binary.expr.parseChunk(bytes))
          } yield assertTrue(result == value)
        },
        test("Expr roundtrip 2") {
          val value1 = Expr(Chunk(Instr.I32Const(1), Instr.I32Const(2), Instr.IAdd(IntWidth.I32)))
          val value2 = Expr(Chunk(Instr.Return))
          val value  = Chunk(value1, value2)
          for {
            bytes  <- ZIO.fromEither(Binary.vec(Binary.expr).print(value))
            result <- ZIO.fromEither(Binary.vec(Binary.expr).parseChunk(bytes))
          } yield assertTrue(result == value)
        },
        test("Expr roundtrip 3") {
          val value1 = Expr(
            Chunk(Instr.I32Const(1), Instr.Block(BlockType.None, Chunk(Instr.I32Const(2))), Instr.IAdd(IntWidth.I32))
          )
          val value2 = Expr(Chunk(Instr.Return))
          val value  = Chunk(value1, value2)
          for {
            bytes  <- ZIO.fromEither(Binary.vec(Binary.expr).print(value))
            result <- ZIO.fromEither(Binary.vec(Binary.expr).parseChunk(bytes))
          } yield assertTrue(result == value)
        },
        test("Expr roundtrip 4") {
          val value1 = Expr(
            Chunk(
              Instr.I32Const(1),
              Instr.If(BlockType.None, Chunk(Instr.I32Const(2)), Chunk.empty),
              Instr.IAdd(IntWidth.I32)
            )
          )
          val value2 = Expr(Chunk(Instr.Return))
          val value  = Chunk(value1, value2)
          for {
            bytes  <- ZIO.fromEither(Binary.vec(Binary.expr).print(value))
            result <- ZIO.fromEither(Binary.vec(Binary.expr).parseChunk(bytes))
          } yield assertTrue(result == value)
        },
        test("Expr roundtrip 5") {
          val value1 = Expr(
            Chunk(
              Instr.I32Const(1),
              Instr.If(BlockType.None, Chunk(Instr.I32Const(2)), Chunk(Instr.Nop)),
              Instr.IAdd(IntWidth.I32)
            )
          )
          val value2 = Expr(Chunk(Instr.Return))
          val value  = Chunk(value1, value2)
          for {
            bytes  <- ZIO.fromEither(Binary.vec(Binary.expr).print(value))
            result <- ZIO.fromEither(Binary.vec(Binary.expr).parseChunk(bytes))
          } yield assertTrue(result == value)
        },
        test("Expr roundtrip 6") {
          val value =
            Expr(Chunk(Instr.I32Const(1), Instr.Store16(NumType.I32, MemArg(1, 0)), Instr.Br(LabelIdx.fromInt(1))))
          for {
            bytes  <- ZIO.fromEither(Binary.expr.print(value))
            result <- ZIO.fromEither(Binary.expr.parseChunk(bytes))
          } yield assertTrue(result == value)
        },
        test("Expr roundtrip 7") {
          val value1 = Expr(
            Chunk(
              Instr.I32Const(1),
              Instr.Block(
                BlockType.None,
                Chunk(
                  Instr.I32Const(2),
                  Instr.If(BlockType.None, Chunk(Instr.I32Const(3), Instr.Br(LabelIdx.fromInt(1))), Chunk.empty),
                  Instr.Loop(BlockType.None, Chunk(Instr.I32Const(2), Instr.BrIf(LabelIdx.fromInt(0))))
                )
              ),
              Instr.IAdd(IntWidth.I32)
            )
          )
          val value2 = Expr(Chunk(Instr.Return))
          val value  = Chunk(value1, value1, value2, value2)
          for {
            bytes  <- ZIO.fromEither(Binary.vec(Binary.expr).print(value))
            result <- ZIO.fromEither(Binary.vec(Binary.expr).parseChunk(bytes))
          } yield assertTrue(result == value)
        },
        test("Instr roundtrip") {
          check(AstGen.instr) { instr =>
            for {
              bytes  <- ZIO.fromEither(Binary.instr.print(instr))
              result <- ZIO.fromEither(Binary.instr.parseChunk(bytes))
            } yield assertTrue(result == instr)
          }
        } @@ TestAspect.size(1000) @@ TestAspect.samples(10000)
      ),
      test("full module") {
        check(AstGen.module) { module =>
          for {
            bytes  <- ZIO.fromEither(Binary.module.print(module))
            result <- ZIO.fromEither(Binary.module.parseChunk(bytes))
          } yield assertTrue(result == module)
        }
      }
    )
}
