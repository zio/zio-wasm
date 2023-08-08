package zio.wasm.syntax

import zio.{Chunk, ChunkBuilder, NonEmptyChunk}
import zio.parser.*
import zio.parser.Parser.ParserError
import zio.parser.ParserOps
import zio.prelude.*
import zio.wasm.*
import zio.wasm.internal.BinarySyntax.*

import scala.reflect.ClassTag

object Binary {

  // LEB128 code based on https://github.com/facebook/buck/blob/main/third-party/java/dx/src/com/android/dex/Leb128.java

  private[wasm] def writeUnsignedLEB128Int: BinaryWriter[Int] =
    Printer.byValue { (value: Int) =>
      // Unsigned LEB128
      val builder   = ChunkBuilder.make[Byte]()
      var remaining = value >>> 7
      var current   = value
      while (remaining != 0) {
        builder += ((current & 0x7f) | 0x80).toByte
        current = remaining
        remaining >>>= 7
      }
      builder += (current & 0x7f).toByte
      anyBytes.asPrinter(builder.result())
    }

  private[wasm] def writeUnsignedLEB128Long: BinaryWriter[Long] =
    Printer.byValue { (value: Long) =>
      // Unsigned LEB128
      val builder   = ChunkBuilder.make[Byte]()
      var remaining = value >>> 7
      var current   = value
      while (remaining != 0) {
        builder += ((current & 0x7f) | 0x80).toByte
        current = remaining
        remaining >>>= 7
      }
      builder += (current & 0x7f).toByte
      anyBytes.asPrinter(builder.result())
    }

  private[wasm] def writeUnsignedLEB128i128: BinaryWriter[Int128] =
    Printer.byValue { (value: Int128) =>
      // Unsigned LEB128
      val builder   = ChunkBuilder.make[Byte]()
      var remaining = value >>> 7
      var current   = value
      while (remaining != Int128.zero) {
        builder += ((current & 0x7f) | 0x80).toByte
        current = remaining
        remaining >>>= 7
      }
      builder += (current & 0x7f).toByte
      anyBytes.asPrinter(builder.result())
    }

  private[wasm] def readUnsignedLEB128Int: BinaryReader[Int] = {
    def read(result: Int, count: Int): BinaryReader[Int] =
      if (count > 5) {
        Parser.fail(SyntaxError.InvalidLEB128)
      } else {
        anyByte.asParser.flatMap { byte =>
          val cur       = byte & 0xff
          val newResult = result | ((cur & 0x7f) << (7 * count))
          val newCount  = count + 1
          if ((cur & 0x80) == 0x80) {
            read(newResult, newCount)
          } else {
            Parser.succeed(newResult)
          }
        }
      }

    read(0, 0)
  }

  private[wasm] def readUnsignedLEB128Long: BinaryReader[Long] = {
    def read(result: Long, count: Int): BinaryReader[Long] =
      if (count > 10) {
        Parser.fail(SyntaxError.InvalidLEB128)
      } else {
        anyByte.asParser.flatMap { byte =>
          val cur       = byte & 0xff
          val newResult = result | ((cur & 0x7f).toLong << (7 * count))
          val newCount  = count + 1
          if ((cur & 0x80) == 0x80) {
            read(newResult, newCount)
          } else {
            Parser.succeed(newResult)
          }
        }
      }

    read(0, 0)
  }

  private[wasm] def readUnsignedLEB128i128: BinaryReader[Int128] = {
    def read(result: Int128, count: Int): BinaryReader[Int128] =
      if (count > 20) {
        Parser.fail(SyntaxError.InvalidLEB128)
      } else {
        anyByte.asParser.flatMap { byte =>
          val cur       = byte & 0xff
          val newResult = result | (Int128.fromInt(cur & 0x7f) << (7 * count))
          val newCount  = count + 1
          if ((cur & 0x80) == 0x80) {
            read(newResult, newCount)
          } else {
            Parser.succeed(newResult)
          }
        }
      }

    read(Int128.zero, 0)
  }

  private[wasm] def writeSignedLEB128Int: BinaryWriter[Int] =
    Printer.byValue { (value: Int) =>
      // Signed LEB128
      val builder   = ChunkBuilder.make[Byte]()
      var remaining = value >> 7
      var current   = value
      var hasMore   = true
      val end       = if ((value & Int.MinValue) == 0) 0 else -1
      while (hasMore) {
        hasMore = (remaining != end) || ((remaining & 1) != ((current >> 6) & 1))
        builder += ((current & 0x7f) | (if (hasMore) 0x80 else 0)).toByte
        current = remaining
        remaining >>= 7
      }
      anyBytes.asPrinter(builder.result())
    }

  private[wasm] def writeSignedLEB128Long: BinaryWriter[Long] =
    Printer.byValue { (value: Long) =>
      // Signed LEB128
      val builder   = ChunkBuilder.make[Byte]()
      var remaining = value >> 7
      var current   = value
      var hasMore   = true
      val end       = if ((value & Long.MinValue) == 0) 0L else -1L
      while (hasMore) {
        hasMore = (remaining != end) || ((remaining & 1) != ((current >> 6) & 1))
        builder += ((current & 0x7f) | (if (hasMore) 0x80 else 0)).toByte
        current = remaining
        remaining >>= 7
      }
      anyBytes.asPrinter(builder.result())
    }

  private[wasm] def writeSignedLEB128i128: BinaryWriter[Int128] =
    Printer.byValue { (value: Int128) =>
      // Signed LEB128
      val builder   = ChunkBuilder.make[Byte]()
      var remaining = value >> 7
      var current   = value
      var hasMore   = true
      val end       = if ((value & Int128.MinValue) == Int128.zero) Int128.zero else Int128.minus1
      while (hasMore) {
        hasMore = (remaining != end) || ((remaining & 1) != ((current >> 6) & 1))
        builder += ((current & 0x7f) | (if (hasMore) 0x80 else 0)).toByte
        current = remaining
        remaining >>= 7
      }
      anyBytes.asPrinter(builder.result())
    }

  private[wasm] def readSignedLEB128Int: BinaryReader[Int] = {
    def read(result: Int, count: Int, signBits: Int): BinaryReader[(Int, Int)] =
      if (count > 5) {
        Parser.fail(SyntaxError.InvalidLEB128)
      } else {
        anyByte.asParser.flatMap { byte =>
          val cur         = byte & 0xff
          val newResult   = result | ((cur & 0x7f) << (7 * count))
          val newSignBits = signBits << 7
          val newCount    = count + 1
          if ((cur & 0x80) == 0x80) {
            read(newResult, newCount, newSignBits)
          } else {
            Parser.succeed((newResult, newSignBits))
          }
        }
      }

    read(0, 0, -1).map { case (result, signBits) =>
      if (((signBits >> 1) & result) != 0) {
        result | signBits
      } else {
        result
      }
    }
  }

  private[wasm] def readSignedLEB128Long: BinaryReader[Long] = {
    def read(result: Long, count: Int, signBits: Long): BinaryReader[(Long, Long)] =
      if (count > 10) {
        Parser.fail(SyntaxError.InvalidLEB128)
      } else {
        anyByte.asParser.flatMap { byte =>
          val cur         = byte & 0xff
          val newResult   = result | ((cur & 0x7f).toLong << (7 * count))
          val newSignBits = signBits << 7
          val newCount    = count + 1
          if ((cur & 0x80) == 0x80) {
            read(newResult, newCount, newSignBits)
          } else {
            Parser.succeed((newResult, newSignBits))
          }
        }
      }

    read(0L, 0, -1L).map { case (result, signBits) =>
      if (((signBits >> 1) & result) != 0) {
        result | signBits
      } else {
        result
      }
    }
  }

  private[wasm] def readSignedLEB128i128: BinaryReader[Int128] = {
    def read(result: Int128, count: Int, signBits: Int128): BinaryReader[(Int128, Int128)] =
      if (count > 20) {
        Parser.fail(SyntaxError.InvalidLEB128)
      } else {
        anyByte.asParser.flatMap { byte =>
          val cur         = byte & 0xff
          val newResult   = result | (Int128.fromInt((cur & 0x7f)) << (7 * count))
          val newSignBits = signBits << 7
          val newCount    = count + 1
          if ((cur & 0x80) == 0x80) {
            read(newResult, newCount, newSignBits)
          } else {
            Parser.succeed((newResult, newSignBits))
          }
        }
      }

    read(Int128.zero, 0, Int128.minus1).map { case (result, signBits) =>
      if (((signBits >> 1) & result) != Int128.zero) {
        result | signBits
      } else {
        result
      }
    }
  }

  private[wasm] val u32: Syntax[SyntaxError, Byte, Byte, Int]   = readUnsignedLEB128Int <=> writeUnsignedLEB128Int
  private[wasm] val i32: Syntax[SyntaxError, Byte, Byte, Int]   = readSignedLEB128Int <=> writeSignedLEB128Int
  private[wasm] val f32: Syntax[SyntaxError, Byte, Byte, Float] =
    (anyByte ~ anyByte ~ anyByte ~ anyByte).transform(
      { case (b0, b1, b2, b3) =>
        java.lang.Float.intBitsToFloat(
          ((b0.toLong & 0xff) | ((b1.toLong & 0xff) << 8) | ((b2.toLong & 0xff) << 16) | ((b3.toLong & 0xff) << 24)).toInt
        )
      },
      { (f: Float) =>
        val n = java.lang.Float.floatToIntBits(f)
        ((n & 0xff).toByte, ((n >> 8) & 0xff).toByte, ((n >> 16) & 0xff).toByte, ((n >> 24) & 0xff).toByte)
      }
    )

  private[wasm] val u64: Syntax[SyntaxError, Byte, Byte, Long]   = readUnsignedLEB128Long <=> writeUnsignedLEB128Long
  private[wasm] val i64: Syntax[SyntaxError, Byte, Byte, Long]   = readSignedLEB128Long <=> writeSignedLEB128Long
  private[wasm] val f64: Syntax[SyntaxError, Byte, Byte, Double] =
    (anyByte ~ anyByte ~ anyByte ~ anyByte ~ anyByte ~ anyByte ~ anyByte ~ anyByte).transform(
      { case (b0, b1, b2, b3, b4, b5, b6, b7) =>
        java.lang.Double.longBitsToDouble(
          ((b0.toLong & 0xff) | ((b1.toLong & 0xff) << 8) | ((b2.toLong & 0xff) << 16) | ((b3.toLong & 0xff) << 24) |
            ((b4.toLong & 0xff) << 32) | ((b5.toLong & 0xff) << 40) | ((b6.toLong & 0xff) << 48) | ((b7.toLong & 0xff) << 56))
        )
      },
      { (f: Double) =>
        val n = java.lang.Double.doubleToLongBits(f)
        (
          (n & 0xff).toByte,
          ((n >> 8) & 0xff).toByte,
          ((n >> 16) & 0xff).toByte,
          ((n >> 24) & 0xff).toByte,
          ((n >> 32) & 0xff).toByte,
          ((n >> 40) & 0xff).toByte,
          ((n >> 48) & 0xff).toByte,
          ((n >> 56) & 0xff).toByte
        )
      }
    )

  private[wasm] val u128: Syntax[SyntaxError, Byte, Byte, Int128] = readUnsignedLEB128i128 <=> writeUnsignedLEB128i128
  private[wasm] val i128: Syntax[SyntaxError, Byte, Byte, Int128] = readSignedLEB128i128 <=> writeSignedLEB128i128

  private[wasm] def vec[A](elem: BinarySyntax[A]): BinarySyntax[Chunk[A]] = {
    val printer =
      Printer.byValue { (vec: Chunk[A]) =>
        writeUnsignedLEB128Int(vec.size) ~ elem.*.asPrinter(vec)
      }

    val parser = for {
      size <- readUnsignedLEB128Int
      vec  <- elem.asParser.exactly(size)
    } yield vec

    (parser <=> printer) ?? "vec"
  }

  private[wasm] def vec1[A](elem: BinarySyntax[A]): BinarySyntax[NonEmptyChunk[A]] =
    vec[A](elem).transformEither(
      c => NonEmptyChunk.fromChunk(c).toRight(SyntaxError.EmptyVector),
      nec => Right(nec.toChunk)
    )

  private[wasm] val name: BinarySyntax[Name] =
    vec(anyByte).transform(
      bytes => Name.fromBytes(bytes),
      name => name.toBytes
    ) ?? "name"

  private[wasm] val numType: BinarySyntax[NumType] =
    anyByte.transformEither(
      {
        case 0x7f        => Right(NumType.I32)
        case 0x7e        => Right(NumType.I64)
        case 0x7d        => Right(NumType.F32)
        case 0x7c        => Right(NumType.F64)
        case other: Byte => Left(SyntaxError.InvalidNumType(other))
      },
      {
        case NumType.I32 => Right(0x7f)
        case NumType.I64 => Right(0x7e)
        case NumType.F32 => Right(0x7d)
        case NumType.F64 => Right(0x7c)
      }
    ) ?? "numType"

  private[wasm] val vecType: BinarySyntax[VecType] =
    anyByte.transformEither(
      {
        case 0x7b => Right(VecType.V128)
        case _    => Left(SyntaxError.InvalidVecType)
      },
      { case VecType.V128 => Right(0x7b) }
    ) ?? "vecType"

  private[wasm] val refType: BinarySyntax[RefType] =
    anyByte.transformEither(
      {
        case 0x70 => Right(RefType.FuncRef)
        case 0x6f => Right(RefType.ExternRef)
        case _    => Left(SyntaxError.InvalidRefType)
      },
      {
        case RefType.FuncRef   => Right(0x70)
        case RefType.ExternRef => Right(0x6f)
      }
    ) ?? "refType"

  private[wasm] val valType: BinarySyntax[ValType] =
    refType.orElseU(numType).orElseU(vecType) ?? "valType"

  private[wasm] val resultType: BinarySyntax[ResultType] =
    vec(valType).transform(
      ResultType.apply,
      _.values
    ) ?? "resultType"

  private[wasm] val funcType: BinarySyntax[FuncType] =
    (specificByte(0x60) ~ resultType ~ resultType).transform(
      { case (_, params, results) => FuncType(params, results) },
      { case FuncType(params, results) => (0x60, params, results) }
    ) ?? "funcType"

  private[wasm] val `type`: BinarySyntax[Type] =
    funcType.transformEither(
      t => Right(Type.Func(t)),
      {
        case Type.Func(t) => Right(t)
        case _            => Left(SyntaxError.InvalidCase)
      }
    ) ?? "type"

  private[wasm] val limits: BinarySyntax[Limits] =
    (specificByte(0x00) ~ u32)
      .transformEither(
        { case (_, min) => Right(Limits(min, None)) },
        {
          case Limits(min, None)  => Right((0x00, min))
          case Limits(_, Some(_)) => Left(SyntaxError.InvalidCase)
        }
      )
      .orElse(
        (specificByte(0x01) ~ u32 ~ u32).transformEither(
          { case (_, min, max) => Right(Limits(min, Some(max))) },
          {
            case Limits(min, Some(max)) => Right((0x01, min, max))
            case Limits(_, None)        => Left(SyntaxError.InvalidCase)
          }
        )
      ) ?? "limits"

  private[wasm] val memoryType: BinarySyntax[MemType] =
    limits.of[MemType] ?? "memoryType"

  private[wasm] val tableType: BinarySyntax[TableType] =
    (refType ~ limits).transform(
      { case (elemType, limits) =>
        TableType(limits, elemType)
      },
      { case TableType(elemType, limits) =>
        (limits, elemType)
      }
    ) ?? "tableType"

  private[wasm] val mut: BinarySyntax[Mut] =
    anyByte.transformEither(
      {
        case 0x00 => Right(Mut.Const)
        case 0x01 => Right(Mut.Var)
        case _    => Left(SyntaxError.InvalidMut)
      },
      {
        case Mut.Const => Right(0x00)
        case Mut.Var   => Right(0x01)
      }
    ) ?? "mut"

  private[wasm] val globalType: BinarySyntax[GlobalType] =
    (valType ~ mut).transform(
      { case (valueType, mut) => GlobalType(mut, valueType) },
      { case GlobalType(mut, valueType) => (valueType, mut) }
    ) ?? "globalType"

  private[wasm] val memArg: BinarySyntax[MemArg] =
    (u32 ~ u32).of[MemArg] ?? "memArg"

  private[wasm] val typeIdx: BinarySyntax[TypeIdx] =
    u32.transform(
      TypeIdx.fromInt,
      _.toInt
    ) ?? "typeIdx"

  private[wasm] val funcIdx: BinarySyntax[FuncIdx] =
    u32.transform(
      FuncIdx.fromInt,
      _.toInt
    ) ?? "funcIdx"

  private[wasm] val dataIdx: BinarySyntax[DataIdx] =
    u32.transform(
      DataIdx.fromInt,
      _.toInt
    ) ?? "dataIdx"

  private[wasm] val tableIdx: BinarySyntax[TableIdx] =
    u32.transform(
      TableIdx.fromInt,
      _.toInt
    ) ?? "tableIdx"

  private[wasm] val elemIdx: BinarySyntax[ElemIdx] =
    u32.transform(
      ElemIdx.fromInt,
      _.toInt
    ) ?? "elemIdx"

  private[wasm] val memIdx: BinarySyntax[MemIdx] =
    u32.transform(
      MemIdx.fromInt,
      _.toInt
    ) ?? "memIdx"

  private[wasm] val globalIdx: BinarySyntax[GlobalIdx] =
    u32.transform(
      GlobalIdx.fromInt,
      _.toInt
    ) ?? "globalIdx"

  private[wasm] val localIdx: BinarySyntax[LocalIdx] =
    u32.transform(
      LocalIdx.fromInt,
      _.toInt
    ) ?? "localIdx"

  private[wasm] val labelIdx: BinarySyntax[LabelIdx] =
    u32.transform(
      LabelIdx.fromInt,
      _.toInt
    ) ?? "labelIdx"

  private[wasm] val laneIdx: BinarySyntax[LaneIdx] =
    anyByte.transform(
      LaneIdx.fromByte,
      _.toByte
    ) ?? "laneIdx"

  private[wasm] val blockType: BinarySyntax[BlockType] =
    specificByte(0x40)
      .transformTo(
        b => BlockType.None,
        { case BlockType.None => 0x40 },
        SyntaxError.InvalidCase
      )
      .widenWith(SyntaxError.InvalidCase)
      .orElse(valType.of[BlockType.Value].widenWith(SyntaxError.InvalidCase))
      .orElse(typeIdx.of[BlockType.Index].widenWith(SyntaxError.InvalidCase)) ?? "blockType"

  private object Instructions {
    val read: BinaryReader[Instr] =
      anyByte.asParser.map(_.toInt & 0xff).flatMap {
        case 0x41 => i32.asParser.map(Instr.I32Const.apply)
        case 0x42 => i64.asParser.map(Instr.I64Const.apply)
        case 0x43 => f32.asParser.map(Instr.F32Const.apply)
        case 0x44 => f64.asParser.map(Instr.F64Const.apply)
        case 0x45 => Parser.succeed(Instr.IEqz(IntWidth.I32))
        case 0x50 => Parser.succeed(Instr.IEqz(IntWidth.I64))
        case 0x46 => Parser.succeed(Instr.IEq(IntWidth.I32))
        case 0x51 => Parser.succeed(Instr.IEq(IntWidth.I64))
        case 0x47 => Parser.succeed(Instr.INe(IntWidth.I32))
        case 0x52 => Parser.succeed(Instr.INe(IntWidth.I64))
        case 0x48 => Parser.succeed(Instr.ILt(IntWidth.I32, Signedness.Signed))
        case 0x49 => Parser.succeed(Instr.ILt(IntWidth.I32, Signedness.Unsigned))
        case 0x53 => Parser.succeed(Instr.ILt(IntWidth.I64, Signedness.Signed))
        case 0x54 => Parser.succeed(Instr.ILt(IntWidth.I64, Signedness.Unsigned))
        case 0x4a => Parser.succeed(Instr.IGt(IntWidth.I32, Signedness.Signed))
        case 0x4b => Parser.succeed(Instr.IGt(IntWidth.I32, Signedness.Unsigned))
        case 0x55 => Parser.succeed(Instr.IGt(IntWidth.I64, Signedness.Signed))
        case 0x56 => Parser.succeed(Instr.IGt(IntWidth.I64, Signedness.Unsigned))
        case 0x4c => Parser.succeed(Instr.ILe(IntWidth.I32, Signedness.Signed))
        case 0x4d => Parser.succeed(Instr.ILe(IntWidth.I32, Signedness.Unsigned))
        case 0x57 => Parser.succeed(Instr.ILe(IntWidth.I64, Signedness.Signed))
        case 0x58 => Parser.succeed(Instr.ILe(IntWidth.I64, Signedness.Unsigned))
        case 0x4e => Parser.succeed(Instr.IGe(IntWidth.I32, Signedness.Signed))
        case 0x4f => Parser.succeed(Instr.IGe(IntWidth.I32, Signedness.Unsigned))
        case 0x59 => Parser.succeed(Instr.IGe(IntWidth.I64, Signedness.Signed))
        case 0x5a => Parser.succeed(Instr.IGe(IntWidth.I64, Signedness.Unsigned))
        case 0x5b => Parser.succeed(Instr.FEq(FloatWidth.F32))
        case 0x61 => Parser.succeed(Instr.FEq(FloatWidth.F64))
        case 0x5c => Parser.succeed(Instr.FNe(FloatWidth.F32))
        case 0x62 => Parser.succeed(Instr.FNe(FloatWidth.F64))
        case 0x5d => Parser.succeed(Instr.FLt(FloatWidth.F32))
        case 0x63 => Parser.succeed(Instr.FLt(FloatWidth.F64))
        case 0x5e => Parser.succeed(Instr.FGt(FloatWidth.F32))
        case 0x64 => Parser.succeed(Instr.FGt(FloatWidth.F64))
        case 0x5f => Parser.succeed(Instr.FLe(FloatWidth.F32))
        case 0x65 => Parser.succeed(Instr.FLe(FloatWidth.F64))
        case 0x60 => Parser.succeed(Instr.FGe(FloatWidth.F32))
        case 0x66 => Parser.succeed(Instr.FGe(FloatWidth.F64))
        case 0x67 => Parser.succeed(Instr.IClz(IntWidth.I32))
        case 0x79 => Parser.succeed(Instr.IClz(IntWidth.I64))
        case 0x68 => Parser.succeed(Instr.ICtz(IntWidth.I32))
        case 0x7a => Parser.succeed(Instr.ICtz(IntWidth.I64))
        case 0x69 => Parser.succeed(Instr.IPopCnt(IntWidth.I32))
        case 0x7b => Parser.succeed(Instr.IPopCnt(IntWidth.I64))
        case 0x6a => Parser.succeed(Instr.IAdd(IntWidth.I32))
        case 0x7c => Parser.succeed(Instr.IAdd(IntWidth.I64))
        case 0x6b => Parser.succeed(Instr.ISub(IntWidth.I32))
        case 0x7d => Parser.succeed(Instr.ISub(IntWidth.I64))
        case 0x6c => Parser.succeed(Instr.IMul(IntWidth.I32))
        case 0x7e => Parser.succeed(Instr.IMul(IntWidth.I64))
        case 0x6d => Parser.succeed(Instr.IDiv(IntWidth.I32, Signedness.Signed))
        case 0x6e => Parser.succeed(Instr.IDiv(IntWidth.I32, Signedness.Unsigned))
        case 0x7f => Parser.succeed(Instr.IDiv(IntWidth.I64, Signedness.Signed))
        case 0x80 => Parser.succeed(Instr.IDiv(IntWidth.I64, Signedness.Unsigned))
        case 0x6f => Parser.succeed(Instr.IRem(IntWidth.I32, Signedness.Signed))
        case 0x70 => Parser.succeed(Instr.IRem(IntWidth.I32, Signedness.Unsigned))
        case 0x81 => Parser.succeed(Instr.IRem(IntWidth.I64, Signedness.Signed))
        case 0x82 => Parser.succeed(Instr.IRem(IntWidth.I64, Signedness.Unsigned))
        case 0x71 => Parser.succeed(Instr.IAnd(IntWidth.I32))
        case 0x83 => Parser.succeed(Instr.IAnd(IntWidth.I64))
        case 0x72 => Parser.succeed(Instr.IOr(IntWidth.I32))
        case 0x84 => Parser.succeed(Instr.IOr(IntWidth.I64))
        case 0x73 => Parser.succeed(Instr.IXor(IntWidth.I32))
        case 0x85 => Parser.succeed(Instr.IXor(IntWidth.I64))
        case 0x74 => Parser.succeed(Instr.IShl(IntWidth.I32))
        case 0x86 => Parser.succeed(Instr.IShl(IntWidth.I64))
        case 0x75 => Parser.succeed(Instr.IShr(IntWidth.I32, Signedness.Signed))
        case 0x76 => Parser.succeed(Instr.IShr(IntWidth.I32, Signedness.Unsigned))
        case 0x87 => Parser.succeed(Instr.IShr(IntWidth.I64, Signedness.Signed))
        case 0x88 => Parser.succeed(Instr.IShr(IntWidth.I64, Signedness.Unsigned))
        case 0x77 => Parser.succeed(Instr.IRotL(IntWidth.I32))
        case 0x89 => Parser.succeed(Instr.IRotL(IntWidth.I64))
        case 0x78 => Parser.succeed(Instr.IRotR(IntWidth.I32))
        case 0x8a => Parser.succeed(Instr.IRotR(IntWidth.I64))
        case 0x8b => Parser.succeed(Instr.FAbs(FloatWidth.F32))
        case 0x99 => Parser.succeed(Instr.FAbs(FloatWidth.F64))
        case 0x8c => Parser.succeed(Instr.FNeg(FloatWidth.F32))
        case 0x9a => Parser.succeed(Instr.FNeg(FloatWidth.F64))
        case 0x8d => Parser.succeed(Instr.FCeil(FloatWidth.F32))
        case 0x9b => Parser.succeed(Instr.FCeil(FloatWidth.F64))
        case 0x8e => Parser.succeed(Instr.FFloor(FloatWidth.F32))
        case 0x9c => Parser.succeed(Instr.FFloor(FloatWidth.F64))
        case 0x8f => Parser.succeed(Instr.FTrunc(FloatWidth.F32))
        case 0x9d => Parser.succeed(Instr.FTrunc(FloatWidth.F64))
        case 0x90 => Parser.succeed(Instr.FNearest(FloatWidth.F32))
        case 0x9e => Parser.succeed(Instr.FNearest(FloatWidth.F64))
        case 0x91 => Parser.succeed(Instr.FSqrt(FloatWidth.F32))
        case 0x9f => Parser.succeed(Instr.FSqrt(FloatWidth.F64))
        case 0x92 => Parser.succeed(Instr.FAdd(FloatWidth.F32))
        case 0xa0 => Parser.succeed(Instr.FAdd(FloatWidth.F64))
        case 0x93 => Parser.succeed(Instr.FSub(FloatWidth.F32))
        case 0xa1 => Parser.succeed(Instr.FSub(FloatWidth.F64))
        case 0x94 => Parser.succeed(Instr.FMul(FloatWidth.F32))
        case 0xa2 => Parser.succeed(Instr.FMul(FloatWidth.F64))
        case 0x95 => Parser.succeed(Instr.FDiv(FloatWidth.F32))
        case 0xa3 => Parser.succeed(Instr.FDiv(FloatWidth.F64))
        case 0x96 => Parser.succeed(Instr.FMin(FloatWidth.F32))
        case 0xa4 => Parser.succeed(Instr.FMin(FloatWidth.F64))
        case 0x97 => Parser.succeed(Instr.FMax(FloatWidth.F32))
        case 0xa5 => Parser.succeed(Instr.FMax(FloatWidth.F64))
        case 0x98 => Parser.succeed(Instr.FCopySign(FloatWidth.F32))
        case 0xa6 => Parser.succeed(Instr.FCopySign(FloatWidth.F64))
        case 0xa7 => Parser.succeed(Instr.I32WrapI64)
        case 0xa8 => Parser.succeed(Instr.ITruncF(IntWidth.I32, FloatWidth.F32, Signedness.Signed))
        case 0xa9 => Parser.succeed(Instr.ITruncF(IntWidth.I32, FloatWidth.F32, Signedness.Unsigned))
        case 0xaa => Parser.succeed(Instr.ITruncF(IntWidth.I32, FloatWidth.F64, Signedness.Signed))
        case 0xab => Parser.succeed(Instr.ITruncF(IntWidth.I32, FloatWidth.F64, Signedness.Unsigned))
        case 0xae => Parser.succeed(Instr.ITruncF(IntWidth.I64, FloatWidth.F32, Signedness.Signed))
        case 0xaf => Parser.succeed(Instr.ITruncF(IntWidth.I64, FloatWidth.F32, Signedness.Unsigned))
        case 0xb0 => Parser.succeed(Instr.ITruncF(IntWidth.I64, FloatWidth.F64, Signedness.Signed))
        case 0xb1 => Parser.succeed(Instr.ITruncF(IntWidth.I64, FloatWidth.F64, Signedness.Unsigned))
        case 0xac => Parser.succeed(Instr.I64ExtendI32(Signedness.Signed))
        case 0xad => Parser.succeed(Instr.I64ExtendI32(Signedness.Unsigned))
        case 0xc4 => Parser.succeed(Instr.I64Extend32S)
        case 0xc0 => Parser.succeed(Instr.IExtend8S(IntWidth.I32))
        case 0xc2 => Parser.succeed(Instr.IExtend8S(IntWidth.I64))
        case 0xc1 => Parser.succeed(Instr.IExtend16S(IntWidth.I32))
        case 0xc3 => Parser.succeed(Instr.IExtend16S(IntWidth.I64))
        case 0xb2 => Parser.succeed(Instr.FConvertI(FloatWidth.F32, IntWidth.I32, Signedness.Signed))
        case 0xb3 => Parser.succeed(Instr.FConvertI(FloatWidth.F32, IntWidth.I32, Signedness.Unsigned))
        case 0xb4 => Parser.succeed(Instr.FConvertI(FloatWidth.F32, IntWidth.I64, Signedness.Signed))
        case 0xb5 => Parser.succeed(Instr.FConvertI(FloatWidth.F32, IntWidth.I64, Signedness.Unsigned))
        case 0xb7 => Parser.succeed(Instr.FConvertI(FloatWidth.F64, IntWidth.I32, Signedness.Signed))
        case 0xb8 => Parser.succeed(Instr.FConvertI(FloatWidth.F64, IntWidth.I32, Signedness.Unsigned))
        case 0xb9 => Parser.succeed(Instr.FConvertI(FloatWidth.F64, IntWidth.I64, Signedness.Signed))
        case 0xba => Parser.succeed(Instr.FConvertI(FloatWidth.F64, IntWidth.I64, Signedness.Unsigned))
        case 0xb6 => Parser.succeed(Instr.F32DemoteF64)
        case 0xbb => Parser.succeed(Instr.F64PromoteF32)
        case 0xbc => Parser.succeed(Instr.IReinterpretF(IntWidth.I32))
        case 0xbd => Parser.succeed(Instr.IReinterpretF(IntWidth.I64))
        case 0xbe => Parser.succeed(Instr.FReinterpretI(FloatWidth.F32))
        case 0xbf => Parser.succeed(Instr.FReinterpretI(FloatWidth.F64))
        case 0xfc =>
          u32.asParser.flatMap {
            case 0          => Parser.succeed(Instr.ITruncSatF(IntWidth.I32, FloatWidth.F32, Signedness.Signed))
            case 1          => Parser.succeed(Instr.ITruncSatF(IntWidth.I32, FloatWidth.F32, Signedness.Unsigned))
            case 2          => Parser.succeed(Instr.ITruncSatF(IntWidth.I32, FloatWidth.F64, Signedness.Signed))
            case 3          => Parser.succeed(Instr.ITruncSatF(IntWidth.I32, FloatWidth.F64, Signedness.Unsigned))
            case 4          => Parser.succeed(Instr.ITruncSatF(IntWidth.I64, FloatWidth.F32, Signedness.Signed))
            case 5          => Parser.succeed(Instr.ITruncSatF(IntWidth.I64, FloatWidth.F32, Signedness.Unsigned))
            case 6          => Parser.succeed(Instr.ITruncSatF(IntWidth.I64, FloatWidth.F64, Signedness.Signed))
            case 7          => Parser.succeed(Instr.ITruncSatF(IntWidth.I64, FloatWidth.F64, Signedness.Unsigned))
            case 16         => Binary.tableIdx.asParser.to[Instr.TableSize]
            case 15         => Binary.tableIdx.asParser.to[Instr.TableGrow]
            case 17         => Binary.tableIdx.asParser.to[Instr.TableFill]
            case 14         => (Binary.tableIdx ~ Binary.tableIdx).asParser.to[Instr.TableCopy]
            case 12         => (Binary.tableIdx ~ Binary.elemIdx).asParser.to[Instr.TableInit]
            case 13         => Binary.elemIdx.asParser.to[Instr.ElemDrop]
            case 11         => specificByte(0x00).asParser.as(Instr.MemoryFill)
            case 10         => (specificByte(0x00) ~ specificByte(0x00)).asParser.as(Instr.MemoryCopy)
            case 8          => (Binary.dataIdx <~ specificByte_(0x00)).asParser.to[Instr.MemoryInit]
            case 9          => Binary.dataIdx.asParser.to[Instr.DataDrop]
            case other: Int => Parser.fail(SyntaxError.InvalidOpcode(Some(0xfc.toByte), other))
          }
        case 0xfd =>
          u32.asParser.flatMap {
            case 12  => i128.asParser.map(Instr.V128Const.apply)
            case 77  => Parser.succeed(Instr.V128Not)
            case 78  => Parser.succeed(Instr.V128And)
            case 79  => Parser.succeed(Instr.V128AndNot)
            case 80  => Parser.succeed(Instr.V128Or)
            case 81  => Parser.succeed(Instr.V128XOr)
            case 82  => Parser.succeed(Instr.V128BitSelect)
            case 83  => Parser.succeed(Instr.V128AnyTrue)
            case 13  =>
              (laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser ~ laneIdx.asParser)
                .to[Instr.VI8x16Shuffle]
            case 14  => Parser.succeed(Instr.VI18x16Swizzle)
            case 15  => Parser.succeed(Instr.VSplat(IShape.I8x16))
            case 16  => Parser.succeed(Instr.VSplat(IShape.I16x8))
            case 17  => Parser.succeed(Instr.VSplat(IShape.I32x4))
            case 18  => Parser.succeed(Instr.VSplat(IShape.I64x2))
            case 19  => Parser.succeed(Instr.VSplat(FShape.F32x4))
            case 20  => Parser.succeed(Instr.VSplat(FShape.F64x2))
            case 21  => laneIdx.asParser.map(laneIdx => Instr.VI8x16ExtractLane(Signedness.Signed, laneIdx))
            case 22  => laneIdx.asParser.map(laneIdx => Instr.VI8x16ExtractLane(Signedness.Unsigned, laneIdx))
            case 24  => laneIdx.asParser.map(laneIdx => Instr.VI16x8ExtractLane(Signedness.Signed, laneIdx))
            case 25  => laneIdx.asParser.map(laneIdx => Instr.VI16x8ExtractLane(Signedness.Unsigned, laneIdx))
            case 27  => laneIdx.asParser.map(laneIdx => Instr.VI32x4ExtractLane(laneIdx))
            case 29  => laneIdx.asParser.map(laneIdx => Instr.VI64x2ExtractLane(laneIdx))
            case 31  => laneIdx.asParser.map(laneIdx => Instr.VFExtractLane(FShape.F32x4, laneIdx))
            case 33  => laneIdx.asParser.map(laneIdx => Instr.VFExtractLane(FShape.F64x2, laneIdx))
            case 23  => laneIdx.asParser.map(laneIdx => Instr.VReplaceLane(IShape.I8x16, laneIdx))
            case 26  => laneIdx.asParser.map(laneIdx => Instr.VReplaceLane(IShape.I16x8, laneIdx))
            case 28  => laneIdx.asParser.map(laneIdx => Instr.VReplaceLane(IShape.I32x4, laneIdx))
            case 30  => laneIdx.asParser.map(laneIdx => Instr.VReplaceLane(IShape.I64x2, laneIdx))
            case 32  => laneIdx.asParser.map(laneIdx => Instr.VReplaceLane(FShape.F32x4, laneIdx))
            case 34  => laneIdx.asParser.map(laneIdx => Instr.VReplaceLane(FShape.F64x2, laneIdx))
            case 35  => Parser.succeed(Instr.VIEq(IShape.I8x16))
            case 45  => Parser.succeed(Instr.VIEq(IShape.I16x8))
            case 55  => Parser.succeed(Instr.VIEq(IShape.I32x4))
            case 214 => Parser.succeed(Instr.VIEq(IShape.I64x2))
            case 36  => Parser.succeed(Instr.VINe(IShape.I8x16))
            case 46  => Parser.succeed(Instr.VINe(IShape.I16x8))
            case 56  => Parser.succeed(Instr.VINe(IShape.I32x4))
            case 215 => Parser.succeed(Instr.VINe(IShape.I64x2))
            case 37  => Parser.succeed(Instr.VILt(IShape.I8x16, Signedness.Signed))
            case 38  => Parser.succeed(Instr.VILt(IShape.I8x16, Signedness.Unsigned))
            case 47  => Parser.succeed(Instr.VILt(IShape.I16x8, Signedness.Signed))
            case 48  => Parser.succeed(Instr.VILt(IShape.I16x8, Signedness.Unsigned))
            case 57  => Parser.succeed(Instr.VILt(IShape.I32x4, Signedness.Signed))
            case 58  => Parser.succeed(Instr.VILt(IShape.I32x4, Signedness.Unsigned))
            case 39  => Parser.succeed(Instr.VIGt(IShape.I8x16, Signedness.Signed))
            case 40  => Parser.succeed(Instr.VIGt(IShape.I8x16, Signedness.Unsigned))
            case 49  => Parser.succeed(Instr.VIGt(IShape.I16x8, Signedness.Signed))
            case 50  => Parser.succeed(Instr.VIGt(IShape.I16x8, Signedness.Unsigned))
            case 59  => Parser.succeed(Instr.VIGt(IShape.I32x4, Signedness.Signed))
            case 60  => Parser.succeed(Instr.VIGt(IShape.I32x4, Signedness.Unsigned))
            case 41  => Parser.succeed(Instr.VILe(IShape.I8x16, Signedness.Signed))
            case 42  => Parser.succeed(Instr.VILe(IShape.I8x16, Signedness.Unsigned))
            case 51  => Parser.succeed(Instr.VILe(IShape.I16x8, Signedness.Signed))
            case 52  => Parser.succeed(Instr.VILe(IShape.I16x8, Signedness.Unsigned))
            case 61  => Parser.succeed(Instr.VILe(IShape.I32x4, Signedness.Signed))
            case 62  => Parser.succeed(Instr.VILe(IShape.I32x4, Signedness.Unsigned))
            case 43  => Parser.succeed(Instr.VIGe(IShape.I8x16, Signedness.Signed))
            case 44  => Parser.succeed(Instr.VIGe(IShape.I8x16, Signedness.Unsigned))
            case 53  => Parser.succeed(Instr.VIGe(IShape.I16x8, Signedness.Signed))
            case 54  => Parser.succeed(Instr.VIGe(IShape.I16x8, Signedness.Unsigned))
            case 63  => Parser.succeed(Instr.VIGe(IShape.I32x4, Signedness.Signed))
            case 64  => Parser.succeed(Instr.VIGe(IShape.I32x4, Signedness.Unsigned))
            case 216 => Parser.succeed(Instr.VI64x2Lt)
            case 217 => Parser.succeed(Instr.VI64x2Gt)
            case 218 => Parser.succeed(Instr.VI64x2Le)
            case 219 => Parser.succeed(Instr.VI64x2Ge)
            case 65  => Parser.succeed(Instr.VFEq(FShape.F32x4))
            case 71  => Parser.succeed(Instr.VFEq(FShape.F64x2))
            case 66  => Parser.succeed(Instr.VFNe(FShape.F32x4))
            case 72  => Parser.succeed(Instr.VFNe(FShape.F64x2))
            case 67  => Parser.succeed(Instr.VFLt(FShape.F32x4))
            case 73  => Parser.succeed(Instr.VFLt(FShape.F64x2))
            case 68  => Parser.succeed(Instr.VFGt(FShape.F32x4))
            case 74  => Parser.succeed(Instr.VFGt(FShape.F64x2))
            case 69  => Parser.succeed(Instr.VFLe(FShape.F32x4))
            case 75  => Parser.succeed(Instr.VFLe(FShape.F64x2))
            case 70  => Parser.succeed(Instr.VFGe(FShape.F32x4))
            case 76  => Parser.succeed(Instr.VFGe(FShape.F64x2))
            case 96  => Parser.succeed(Instr.VIAbs(IShape.I8x16))
            case 128 => Parser.succeed(Instr.VIAbs(IShape.I16x8))
            case 160 => Parser.succeed(Instr.VIAbs(IShape.I32x4))
            case 192 => Parser.succeed(Instr.VIAbs(IShape.I64x2))
            case 97  => Parser.succeed(Instr.VINeg(IShape.I8x16))
            case 129 => Parser.succeed(Instr.VINeg(IShape.I16x8))
            case 161 => Parser.succeed(Instr.VINeg(IShape.I32x4))
            case 193 => Parser.succeed(Instr.VINeg(IShape.I64x2))
            case 98  => Parser.succeed(Instr.VI8x16PopCnt)
            case 130 => Parser.succeed(Instr.VI16x8Q15MulrSat)
            case 186 => Parser.succeed(Instr.VI32x4DotI16x8)
            case 224 => Parser.succeed(Instr.VFAbs(FShape.F32x4))
            case 236 => Parser.succeed(Instr.VFAbs(FShape.F64x2))
            case 225 => Parser.succeed(Instr.VFNeg(FShape.F32x4))
            case 237 => Parser.succeed(Instr.VFNeg(FShape.F64x2))
            case 227 => Parser.succeed(Instr.VFSqrt(FShape.F32x4))
            case 239 => Parser.succeed(Instr.VFSqrt(FShape.F64x2))
            case 103 => Parser.succeed(Instr.VFCeil(FShape.F32x4))
            case 116 => Parser.succeed(Instr.VFCeil(FShape.F64x2))
            case 104 => Parser.succeed(Instr.VFFloor(FShape.F32x4))
            case 117 => Parser.succeed(Instr.VFFloor(FShape.F64x2))
            case 105 => Parser.succeed(Instr.VFTrunc(FShape.F32x4))
            case 122 => Parser.succeed(Instr.VFTrunc(FShape.F64x2))
            case 106 => Parser.succeed(Instr.VFNearest(FShape.F32x4))
            case 148 => Parser.succeed(Instr.VFNearest(FShape.F64x2))
            case 99  => Parser.succeed(Instr.VIAllTrue(IShape.I8x16))
            case 131 => Parser.succeed(Instr.VIAllTrue(IShape.I16x8))
            case 163 => Parser.succeed(Instr.VIAllTrue(IShape.I32x4))
            case 195 => Parser.succeed(Instr.VIAllTrue(IShape.I64x2))
            case 100 => Parser.succeed(Instr.VIBitMask(IShape.I8x16))
            case 132 => Parser.succeed(Instr.VIBitMask(IShape.I16x8))
            case 164 => Parser.succeed(Instr.VIBitMask(IShape.I32x4))
            case 196 => Parser.succeed(Instr.VIBitMask(IShape.I64x2))
            case 101 => Parser.succeed(Instr.VI8x16NarrowI16x8(Signedness.Signed))
            case 102 => Parser.succeed(Instr.VI8x16NarrowI16x8(Signedness.Unsigned))
            case 133 => Parser.succeed(Instr.VI16x8NarrowI32x4(Signedness.Signed))
            case 134 => Parser.succeed(Instr.VI16x8NarrowI32x4(Signedness.Unsigned))
            case 135 => Parser.succeed(Instr.VI16x8ExtendI8x16(Half.Low, Signedness.Signed))
            case 136 => Parser.succeed(Instr.VI16x8ExtendI8x16(Half.High, Signedness.Signed))
            case 137 => Parser.succeed(Instr.VI16x8ExtendI8x16(Half.Low, Signedness.Unsigned))
            case 138 => Parser.succeed(Instr.VI16x8ExtendI8x16(Half.High, Signedness.Unsigned))
            case 167 => Parser.succeed(Instr.VI32x4ExtendI16x8(Half.Low, Signedness.Signed))
            case 168 => Parser.succeed(Instr.VI32x4ExtendI16x8(Half.High, Signedness.Signed))
            case 169 => Parser.succeed(Instr.VI32x4ExtendI16x8(Half.Low, Signedness.Unsigned))
            case 170 => Parser.succeed(Instr.VI32x4ExtendI16x8(Half.High, Signedness.Unsigned))
            case 199 => Parser.succeed(Instr.VI64x2ExtendI32x4(Half.Low, Signedness.Signed))
            case 200 => Parser.succeed(Instr.VI64x2ExtendI32x4(Half.High, Signedness.Signed))
            case 201 => Parser.succeed(Instr.VI64x2ExtendI32x4(Half.Low, Signedness.Unsigned))
            case 202 => Parser.succeed(Instr.VI64x2ExtendI32x4(Half.High, Signedness.Unsigned))
            case 107 => Parser.succeed(Instr.VIShl(IShape.I8x16))
            case 139 => Parser.succeed(Instr.VIShl(IShape.I16x8))
            case 171 => Parser.succeed(Instr.VIShl(IShape.I32x4))
            case 203 => Parser.succeed(Instr.VIShl(IShape.I64x2))
            case 108 => Parser.succeed(Instr.VIShr(IShape.I8x16, Signedness.Signed))
            case 109 => Parser.succeed(Instr.VIShr(IShape.I8x16, Signedness.Unsigned))
            case 140 => Parser.succeed(Instr.VIShr(IShape.I16x8, Signedness.Signed))
            case 141 => Parser.succeed(Instr.VIShr(IShape.I16x8, Signedness.Unsigned))
            case 172 => Parser.succeed(Instr.VIShr(IShape.I32x4, Signedness.Signed))
            case 173 => Parser.succeed(Instr.VIShr(IShape.I32x4, Signedness.Unsigned))
            case 204 => Parser.succeed(Instr.VIShr(IShape.I64x2, Signedness.Signed))
            case 205 => Parser.succeed(Instr.VIShr(IShape.I64x2, Signedness.Unsigned))
            case 110 => Parser.succeed(Instr.VIAdd(IShape.I8x16))
            case 142 => Parser.succeed(Instr.VIAdd(IShape.I16x8))
            case 174 => Parser.succeed(Instr.VIAdd(IShape.I32x4))
            case 206 => Parser.succeed(Instr.VIAdd(IShape.I64x2))
            case 113 => Parser.succeed(Instr.VISub(IShape.I8x16))
            case 145 => Parser.succeed(Instr.VISub(IShape.I16x8))
            case 177 => Parser.succeed(Instr.VISub(IShape.I32x4))
            case 209 => Parser.succeed(Instr.VISub(IShape.I64x2))
            case 118 => Parser.succeed(Instr.VIMin(IShape.I8x16, Signedness.Signed))
            case 119 => Parser.succeed(Instr.VIMin(IShape.I8x16, Signedness.Unsigned))
            case 150 => Parser.succeed(Instr.VIMin(IShape.I16x8, Signedness.Signed))
            case 151 => Parser.succeed(Instr.VIMin(IShape.I16x8, Signedness.Unsigned))
            case 182 => Parser.succeed(Instr.VIMin(IShape.I32x4, Signedness.Signed))
            case 183 => Parser.succeed(Instr.VIMin(IShape.I32x4, Signedness.Unsigned))
            case 120 => Parser.succeed(Instr.VIMax(IShape.I8x16, Signedness.Signed))
            case 121 => Parser.succeed(Instr.VIMax(IShape.I8x16, Signedness.Unsigned))
            case 152 => Parser.succeed(Instr.VIMax(IShape.I16x8, Signedness.Signed))
            case 153 => Parser.succeed(Instr.VIMax(IShape.I16x8, Signedness.Unsigned))
            case 184 => Parser.succeed(Instr.VIMax(IShape.I32x4, Signedness.Signed))
            case 185 => Parser.succeed(Instr.VIMax(IShape.I32x4, Signedness.Unsigned))
            case 111 => Parser.succeed(Instr.VIAddSat(IShape.I8x16, Signedness.Signed))
            case 112 => Parser.succeed(Instr.VIAddSat(IShape.I8x16, Signedness.Unsigned))
            case 143 => Parser.succeed(Instr.VIAddSat(IShape.I16x8, Signedness.Signed))
            case 144 => Parser.succeed(Instr.VIAddSat(IShape.I16x8, Signedness.Unsigned))
            case 114 => Parser.succeed(Instr.VISubSat(IShape.I8x16, Signedness.Signed))
            case 115 => Parser.succeed(Instr.VISubSat(IShape.I8x16, Signedness.Unsigned))
            case 146 => Parser.succeed(Instr.VISubSat(IShape.I16x8, Signedness.Signed))
            case 147 => Parser.succeed(Instr.VISubSat(IShape.I16x8, Signedness.Unsigned))
            case 149 => Parser.succeed(Instr.VIMul(IShape.I16x8))
            case 181 => Parser.succeed(Instr.VIMul(IShape.I32x4))
            case 213 => Parser.succeed(Instr.VIMul(IShape.I64x2))
            case 123 => Parser.succeed(Instr.VIAvgr(IShape.I8x16))
            case 155 => Parser.succeed(Instr.VIAvgr(IShape.I16x8))
            case 156 => Parser.succeed(Instr.VIExtMul(IShape.I16x8, Half.Low, Signedness.Signed))
            case 157 => Parser.succeed(Instr.VIExtMul(IShape.I16x8, Half.High, Signedness.Signed))
            case 158 => Parser.succeed(Instr.VIExtMul(IShape.I16x8, Half.Low, Signedness.Unsigned))
            case 159 => Parser.succeed(Instr.VIExtMul(IShape.I16x8, Half.High, Signedness.Unsigned))
            case 188 => Parser.succeed(Instr.VIExtMul(IShape.I32x4, Half.Low, Signedness.Signed))
            case 189 => Parser.succeed(Instr.VIExtMul(IShape.I32x4, Half.High, Signedness.Signed))
            case 190 => Parser.succeed(Instr.VIExtMul(IShape.I32x4, Half.Low, Signedness.Unsigned))
            case 191 => Parser.succeed(Instr.VIExtMul(IShape.I32x4, Half.High, Signedness.Unsigned))
            case 220 => Parser.succeed(Instr.VIExtMul(IShape.I64x2, Half.Low, Signedness.Signed))
            case 221 => Parser.succeed(Instr.VIExtMul(IShape.I64x2, Half.High, Signedness.Signed))
            case 222 => Parser.succeed(Instr.VIExtMul(IShape.I64x2, Half.Low, Signedness.Unsigned))
            case 223 => Parser.succeed(Instr.VIExtMul(IShape.I64x2, Half.High, Signedness.Unsigned))
            case 124 => Parser.succeed(Instr.VIExtAddPairwise(IShape.I16x8, Signedness.Signed))
            case 125 => Parser.succeed(Instr.VIExtAddPairwise(IShape.I16x8, Signedness.Unsigned))
            case 126 => Parser.succeed(Instr.VIExtAddPairwise(IShape.I32x4, Signedness.Signed))
            case 127 => Parser.succeed(Instr.VIExtAddPairwise(IShape.I32x4, Signedness.Unsigned))
            case 228 => Parser.succeed(Instr.VFAdd(FShape.F32x4))
            case 240 => Parser.succeed(Instr.VFAdd(FShape.F64x2))
            case 229 => Parser.succeed(Instr.VFSub(FShape.F32x4))
            case 241 => Parser.succeed(Instr.VFSub(FShape.F64x2))
            case 230 => Parser.succeed(Instr.VFMul(FShape.F32x4))
            case 242 => Parser.succeed(Instr.VFMul(FShape.F64x2))
            case 231 => Parser.succeed(Instr.VFDiv(FShape.F32x4))
            case 243 => Parser.succeed(Instr.VFDiv(FShape.F64x2))
            case 232 => Parser.succeed(Instr.VFMin(FShape.F32x4))
            case 244 => Parser.succeed(Instr.VFMin(FShape.F64x2))
            case 233 => Parser.succeed(Instr.VFMax(FShape.F32x4))
            case 245 => Parser.succeed(Instr.VFMax(FShape.F64x2))
            case 234 => Parser.succeed(Instr.VFPMin(FShape.F32x4))
            case 246 => Parser.succeed(Instr.VFPMin(FShape.F64x2))
            case 235 => Parser.succeed(Instr.VFPMax(FShape.F32x4))
            case 247 => Parser.succeed(Instr.VFPMax(FShape.F64x2))
            case 248 => Parser.succeed(Instr.VI32x4TruncSatF32x4(Signedness.Signed))
            case 249 => Parser.succeed(Instr.VI32x4TruncSatF32x4(Signedness.Unsigned))
            case 252 => Parser.succeed(Instr.VI32x4TruncSatF64x2Zero(Signedness.Signed))
            case 253 => Parser.succeed(Instr.VI32x4TruncSatF64x2Zero(Signedness.Unsigned))
            case 250 => Parser.succeed(Instr.VI32x4ConvertI32x4(Signedness.Signed))
            case 251 => Parser.succeed(Instr.VI32x4ConvertI32x4(Signedness.Unsigned))
            case 94  => Parser.succeed(Instr.VF32x4DemoteF64x2Zero)
            case 254 => Parser.succeed(Instr.VF64x2ConvertLowI32x4(Signedness.Signed))
            case 255 => Parser.succeed(Instr.VF64x2ConvertLowI32x4(Signedness.Unsigned))
            case 95  => Parser.succeed(Instr.VF64x2PromoteLowI32x4)
            case 0   => Binary.memArg.asParser.map(memArg => Instr.Load(VecType.V128, memArg))
            case 11  => Binary.memArg.asParser.map(memArg => Instr.Store(VecType.V128, memArg))
            case 1   => Binary.memArg.asParser.map(memArg => Instr.V128Load8x8(Signedness.Signed, memArg))
            case 2   => Binary.memArg.asParser.map(memArg => Instr.V128Load8x8(Signedness.Unsigned, memArg))
            case 3   => Binary.memArg.asParser.map(memArg => Instr.V128Load16x4(Signedness.Signed, memArg))
            case 4   => Binary.memArg.asParser.map(memArg => Instr.V128Load16x4(Signedness.Unsigned, memArg))
            case 5   => Binary.memArg.asParser.map(memArg => Instr.V128Load32x2(Signedness.Signed, memArg))
            case 6   => Binary.memArg.asParser.map(memArg => Instr.V128Load32x2(Signedness.Unsigned, memArg))
            case 92  => Binary.memArg.asParser.map(memArg => Instr.V128Load32Zero(memArg))
            case 93  => Binary.memArg.asParser.map(memArg => Instr.V128Load64Zero(memArg))
            case 7   => Binary.memArg.asParser.map(memArg => Instr.V128LoadSplat(VectorLoadShape.WW8, memArg))
            case 8   => Binary.memArg.asParser.map(memArg => Instr.V128LoadSplat(VectorLoadShape.WW16, memArg))
            case 9   => Binary.memArg.asParser.map(memArg => Instr.V128LoadSplat(VectorLoadShape.WW32, memArg))
            case 10  => Binary.memArg.asParser.map(memArg => Instr.V128LoadSplat(VectorLoadShape.WW64, memArg))
            case 84  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128LoadLane(VectorLoadShape.WW8, memArg, laneIdx)
              }
            case 85  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128LoadLane(VectorLoadShape.WW16, memArg, laneIdx)
              }
            case 86  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128LoadLane(VectorLoadShape.WW32, memArg, laneIdx)
              }
            case 87  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128LoadLane(VectorLoadShape.WW64, memArg, laneIdx)
              }
            case 88  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128StoreLane(VectorLoadShape.WW8, memArg, laneIdx)
              }
            case 89  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128StoreLane(VectorLoadShape.WW16, memArg, laneIdx)
              }
            case 90  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128StoreLane(VectorLoadShape.WW32, memArg, laneIdx)
              }
            case 91  =>
              (Binary.memArg ~ Binary.laneIdx).asParser.map { case (memArg, laneIdx) =>
                Instr.V128StoreLane(VectorLoadShape.WW64, memArg, laneIdx)
              }

            case other: Int => Parser.fail(SyntaxError.InvalidOpcode(Some(0xfd.toByte), other))
          }
        case 0xd0 => Binary.refType.asParser.to[Instr.RefNull]
        case 0xd1 => Parser.succeed(Instr.RefIsNull)
        case 0xd2 => Binary.funcIdx.asParser.to[Instr.RefFunc]
        case 0x1a => Parser.succeed(Instr.Drop)
        case 0x1c => vec(Binary.valType).asParser.map(values => Instr.Select(Some(values)))
        case 0x1b => Parser.succeed(Instr.Select(None))
        case 0x20 => Binary.localIdx.asParser.to[Instr.LocalGet]
        case 0x21 => Binary.localIdx.asParser.to[Instr.LocalSet]
        case 0x22 => Binary.localIdx.asParser.to[Instr.LocalTee]
        case 0x23 => Binary.globalIdx.asParser.to[Instr.GlobalGet]
        case 0x24 => Binary.globalIdx.asParser.to[Instr.GlobalSet]
        case 0x25 => Binary.tableIdx.asParser.to[Instr.TableGet]
        case 0x26 => Binary.tableIdx.asParser.to[Instr.TableSet]
        case 0x28 => Binary.memArg.asParser.map(memArg => Instr.Load(NumType.I32, memArg))
        case 0x29 => Binary.memArg.asParser.map(memArg => Instr.Load(NumType.I64, memArg))
        case 0x2a => Binary.memArg.asParser.map(memArg => Instr.Load(NumType.F32, memArg))
        case 0x2b => Binary.memArg.asParser.map(memArg => Instr.Load(NumType.F64, memArg))
        case 0x36 => Binary.memArg.asParser.map(memArg => Instr.Store(NumType.I32, memArg))
        case 0x37 => Binary.memArg.asParser.map(memArg => Instr.Store(NumType.I64, memArg))
        case 0x38 => Binary.memArg.asParser.map(memArg => Instr.Store(NumType.F32, memArg))
        case 0x39 => Binary.memArg.asParser.map(memArg => Instr.Store(NumType.F64, memArg))
        case 0x2c => Binary.memArg.asParser.map(memArg => Instr.Load8(NumType.I32, Signedness.Signed, memArg))
        case 0x2d => Binary.memArg.asParser.map(memArg => Instr.Load8(NumType.I32, Signedness.Unsigned, memArg))
        case 0x30 => Binary.memArg.asParser.map(memArg => Instr.Load8(NumType.I64, Signedness.Signed, memArg))
        case 0x31 => Binary.memArg.asParser.map(memArg => Instr.Load8(NumType.I64, Signedness.Unsigned, memArg))
        case 0x2e => Binary.memArg.asParser.map(memArg => Instr.Load16(NumType.I32, Signedness.Signed, memArg))
        case 0x2f => Binary.memArg.asParser.map(memArg => Instr.Load16(NumType.I32, Signedness.Unsigned, memArg))
        case 0x32 => Binary.memArg.asParser.map(memArg => Instr.Load16(NumType.I64, Signedness.Signed, memArg))
        case 0x33 => Binary.memArg.asParser.map(memArg => Instr.Load16(NumType.I64, Signedness.Unsigned, memArg))
        case 0x34 => Binary.memArg.asParser.map(memArg => Instr.Load32(Signedness.Signed, memArg))
        case 0x35 => Binary.memArg.asParser.map(memArg => Instr.Load32(Signedness.Unsigned, memArg))
        case 0x3a => Binary.memArg.asParser.map(memArg => Instr.Store8(NumType.I32, memArg))
        case 0x3c => Binary.memArg.asParser.map(memArg => Instr.Store8(NumType.I64, memArg))
        case 0x3b => Binary.memArg.asParser.map(memArg => Instr.Store16(NumType.I32, memArg))
        case 0x3d => Binary.memArg.asParser.map(memArg => Instr.Store16(NumType.I64, memArg))
        case 0x3e => Binary.memArg.asParser.map(memArg => Instr.Store32(memArg))
        case 0x3f => specificByte(0x00).asParser.as(Instr.MemorySize)
        case 0x40 => specificByte(0x00).asParser.as(Instr.MemoryGrow)
        case 0x01 => Parser.succeed(Instr.Nop)
        case 0x00 => Parser.succeed(Instr.Unreachable)
        case 0x02 =>
          (Binary.blockType ~ Binary.instr
            .repeatUntil(specificByte_(0x0b.toByte).backtrack)).asParser
            .to[Instr.Block]
        case 0x03 =>
          (Binary.blockType ~ Binary.instr
            .repeatUntil(specificByte_(0x0b.toByte).backtrack)).asParser
            .to[Instr.Loop]
        case 0x04 =>
          Binary.blockType.asParser.flatMap { blockType =>
            (Binary.instr <+> anyByte.filter(b => b == 0x05, SyntaxError.UnexpectedByte)).autoBacktracking
              .repeatUntil(specificByte_(0x0b.toByte).backtrack)
              .asParser
              .map { instrs =>
                val (trueInstr, falseInstr) = instrs.splitWhere(_.isRight)
                Instr.If(blockType, trueInstr.collect { case Left(i) => i }, falseInstr.collect { case Left(i) => i })
              }
          }
        case 0x0c => Binary.labelIdx.asParser.to[Instr.Br]
        case 0x0d => Binary.labelIdx.asParser.to[Instr.BrIf]
        case 0x0e => (vec(Binary.labelIdx) ~ Binary.labelIdx).asParser.to[Instr.BrTable]
        case 0x0f => Parser.succeed(Instr.Return)
        case 0x10 => Binary.funcIdx.asParser.to[Instr.Call]
        case 0x11 =>
          (Binary.typeIdx ~ Binary.tableIdx).asParser.map { case (typeIdx, tableIdx) =>
            Instr.CallIndirect(tableIdx, typeIdx)
          }

        case other: Int => Parser.fail(SyntaxError.InvalidOpcode(None, other))
      }

    val write: BinaryWriter[Instr] =
      Printer.byValue { (instr: Instr) =>
        instr match {
          case Instr.I32Const(value)                              => opcode(0x41) ~ i32.asPrinter(value)
          case Instr.I64Const(value)                              => opcode(0x42) ~ i64.asPrinter(value)
          case Instr.F32Const(value)                              => opcode(0x43) ~ f32.asPrinter(value)
          case Instr.F64Const(value)                              => opcode(0x44) ~ f64.asPrinter(value)
          case Instr.IEqz(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x45)
              case IntWidth.I64 => opcode(0x50)
            }
          case Instr.IEq(width)                                   =>
            width match {
              case IntWidth.I32 => opcode(0x46)
              case IntWidth.I64 => opcode(0x51)
            }
          case Instr.INe(width)                                   =>
            width match {
              case IntWidth.I32 => opcode(0x47)
              case IntWidth.I64 => opcode(0x52)
            }
          case Instr.ILt(width, signedness)                       =>
            (width, signedness) match {
              case (IntWidth.I32, Signedness.Signed)   => opcode(0x48)
              case (IntWidth.I32, Signedness.Unsigned) => opcode(0x49)
              case (IntWidth.I64, Signedness.Signed)   => opcode(0x53)
              case (IntWidth.I64, Signedness.Unsigned) => opcode(0x54)
            }
          case Instr.IGt(width, signedness)                       =>
            (width, signedness) match {
              case (IntWidth.I32, Signedness.Signed)   => opcode(0x4a)
              case (IntWidth.I32, Signedness.Unsigned) => opcode(0x4b)
              case (IntWidth.I64, Signedness.Signed)   => opcode(0x55)
              case (IntWidth.I64, Signedness.Unsigned) => opcode(0x56)
            }
          case Instr.ILe(width, signedness)                       =>
            (width, signedness) match {
              case (IntWidth.I32, Signedness.Signed)   => opcode(0x4c)
              case (IntWidth.I32, Signedness.Unsigned) => opcode(0x4d)
              case (IntWidth.I64, Signedness.Signed)   => opcode(0x57)
              case (IntWidth.I64, Signedness.Unsigned) => opcode(0x58)
            }
          case Instr.IGe(width, signedness)                       =>
            (width, signedness) match {
              case (IntWidth.I32, Signedness.Signed)   => opcode(0x4e)
              case (IntWidth.I32, Signedness.Unsigned) => opcode(0x4f)
              case (IntWidth.I64, Signedness.Signed)   => opcode(0x59)
              case (IntWidth.I64, Signedness.Unsigned) => opcode(0x5a)
            }
          case Instr.FEq(width)                                   =>
            width match {
              case FloatWidth.F32 => opcode(0x5b)
              case FloatWidth.F64 => opcode(0x61)
            }
          case Instr.FNe(width)                                   =>
            width match {
              case FloatWidth.F32 => opcode(0x5c)
              case FloatWidth.F64 => opcode(0x62)
            }
          case Instr.FLt(width)                                   =>
            width match {
              case FloatWidth.F32 => opcode(0x5d)
              case FloatWidth.F64 => opcode(0x63)
            }
          case Instr.FGt(width)                                   =>
            width match {
              case FloatWidth.F32 => opcode(0x5e)
              case FloatWidth.F64 => opcode(0x64)
            }
          case Instr.FLe(width)                                   =>
            width match {
              case FloatWidth.F32 => opcode(0x5f)
              case FloatWidth.F64 => opcode(0x65)
            }
          case Instr.FGe(width)                                   =>
            width match {
              case FloatWidth.F32 => opcode(0x60)
              case FloatWidth.F64 => opcode(0x66)
            }
          case Instr.IClz(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x67)
              case IntWidth.I64 => opcode(0x79)
            }
          case Instr.ICtz(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x68)
              case IntWidth.I64 => opcode(0x7a)
            }
          case Instr.IPopCnt(width)                               =>
            width match {
              case IntWidth.I32 => opcode(0x69)
              case IntWidth.I64 => opcode(0x7b)
            }
          case Instr.IAdd(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x6a)
              case IntWidth.I64 => opcode(0x7c)
            }
          case Instr.ISub(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x6b)
              case IntWidth.I64 => opcode(0x7d)
            }
          case Instr.IMul(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x6c)
              case IntWidth.I64 => opcode(0x7e)
            }
          case Instr.IDiv(width, signedness)                      =>
            (width, signedness) match {
              case (IntWidth.I32, Signedness.Signed)   => opcode(0x6d)
              case (IntWidth.I32, Signedness.Unsigned) => opcode(0x6e)
              case (IntWidth.I64, Signedness.Signed)   => opcode(0x7f)
              case (IntWidth.I64, Signedness.Unsigned) => opcode(0x80.toByte)
            }
          case Instr.IRem(width, signedness)                      =>
            (width, signedness) match {
              case (IntWidth.I32, Signedness.Signed)   => opcode(0x6f)
              case (IntWidth.I32, Signedness.Unsigned) => opcode(0x70)
              case (IntWidth.I64, Signedness.Signed)   => opcode(0x81.toByte)
              case (IntWidth.I64, Signedness.Unsigned) => opcode(0x82.toByte)
            }
          case Instr.IAnd(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x71)
              case IntWidth.I64 => opcode(0x83.toByte)
            }
          case Instr.IOr(width)                                   =>
            width match {
              case IntWidth.I32 => opcode(0x72)
              case IntWidth.I64 => opcode(0x84.toByte)
            }
          case Instr.IXor(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x73)
              case IntWidth.I64 => opcode(0x85.toByte)
            }
          case Instr.IShl(width)                                  =>
            width match {
              case IntWidth.I32 => opcode(0x74)
              case IntWidth.I64 => opcode(0x86.toByte)
            }
          case Instr.IShr(width, signedness)                      =>
            (width, signedness) match {
              case (IntWidth.I32, Signedness.Signed)   => opcode(0x75)
              case (IntWidth.I32, Signedness.Unsigned) => opcode(0x76)
              case (IntWidth.I64, Signedness.Signed)   => opcode(0x87.toByte)
              case (IntWidth.I64, Signedness.Unsigned) => opcode(0x88.toByte)
            }
          case Instr.IRotL(width)                                 =>
            width match {
              case IntWidth.I32 => opcode(0x77)
              case IntWidth.I64 => opcode(0x89.toByte)
            }
          case Instr.IRotR(width)                                 =>
            width match {
              case IntWidth.I32 => opcode(0x78)
              case IntWidth.I64 => opcode(0x8a.toByte)
            }
          case Instr.FAbs(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x8b.toByte)
              case FloatWidth.F64 => opcode(0x99.toByte)
            }
          case Instr.FNeg(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x8c.toByte)
              case FloatWidth.F64 => opcode(0x9a.toByte)
            }
          case Instr.FCeil(width)                                 =>
            width match {
              case FloatWidth.F32 => opcode(0x8d.toByte)
              case FloatWidth.F64 => opcode(0x9b.toByte)
            }
          case Instr.FFloor(width)                                =>
            width match {
              case FloatWidth.F32 => opcode(0x8e.toByte)
              case FloatWidth.F64 => opcode(0x9c.toByte)
            }
          case Instr.FTrunc(width)                                =>
            width match {
              case FloatWidth.F32 => opcode(0x8f.toByte)
              case FloatWidth.F64 => opcode(0x9d.toByte)
            }
          case Instr.FNearest(width)                              =>
            width match {
              case FloatWidth.F32 => opcode(0x90.toByte)
              case FloatWidth.F64 => opcode(0x9e.toByte)
            }
          case Instr.FSqrt(width)                                 =>
            width match {
              case FloatWidth.F32 => opcode(0x91.toByte)
              case FloatWidth.F64 => opcode(0x9f.toByte)
            }
          case Instr.FAdd(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x92.toByte)
              case FloatWidth.F64 => opcode(0xa0.toByte)
            }
          case Instr.FSub(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x93.toByte)
              case FloatWidth.F64 => opcode(0xa1.toByte)
            }
          case Instr.FMul(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x94.toByte)
              case FloatWidth.F64 => opcode(0xa2.toByte)
            }
          case Instr.FDiv(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x95.toByte)
              case FloatWidth.F64 => opcode(0xa3.toByte)
            }
          case Instr.FMin(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x96.toByte)
              case FloatWidth.F64 => opcode(0xa4.toByte)
            }
          case Instr.FMax(width)                                  =>
            width match {
              case FloatWidth.F32 => opcode(0x97.toByte)
              case FloatWidth.F64 => opcode(0xa5.toByte)
            }
          case Instr.FCopySign(width)                             =>
            width match {
              case FloatWidth.F32 => opcode(0x98.toByte)
              case FloatWidth.F64 => opcode(0xa6.toByte)
            }
          case Instr.I32WrapI64                                   => opcode(0xa7.toByte)
          case Instr.ITruncF(intWidth, floatWidth, signedness)    =>
            (intWidth, floatWidth, signedness) match {
              case (IntWidth.I32, FloatWidth.F32, Signedness.Signed)   => opcode(0xa8.toByte)
              case (IntWidth.I32, FloatWidth.F32, Signedness.Unsigned) => opcode(0xa9.toByte)
              case (IntWidth.I32, FloatWidth.F64, Signedness.Signed)   => opcode(0xaa.toByte)
              case (IntWidth.I32, FloatWidth.F64, Signedness.Unsigned) => opcode(0xab.toByte)
              case (IntWidth.I64, FloatWidth.F32, Signedness.Signed)   => opcode(0xae.toByte)
              case (IntWidth.I64, FloatWidth.F32, Signedness.Unsigned) => opcode(0xaf.toByte)
              case (IntWidth.I64, FloatWidth.F64, Signedness.Signed)   => opcode(0xb0.toByte)
              case (IntWidth.I64, FloatWidth.F64, Signedness.Unsigned) => opcode(0xb1.toByte)
            }
          case Instr.I64ExtendI32(signedness)                     =>
            signedness match {
              case Signedness.Signed   => opcode(0xac.toByte)
              case Signedness.Unsigned => opcode(0xad.toByte)
            }
          case Instr.I64Extend32S                                 => opcode(0xc4.toByte)
          case Instr.IExtend8S(intWidth)                          =>
            intWidth match {
              case IntWidth.I32 => opcode(0xc0.toByte)
              case IntWidth.I64 => opcode(0xc2.toByte)
            }
          case Instr.IExtend16S(intWidth)                         =>
            intWidth match {
              case IntWidth.I32 => opcode(0xc1.toByte)
              case IntWidth.I64 => opcode(0xc3.toByte)
            }
          case Instr.FConvertI(floatWidth, intWidth, signedness)  =>
            (floatWidth, intWidth, signedness) match {
              case (FloatWidth.F32, IntWidth.I32, Signedness.Signed)   => opcode(0xb2.toByte)
              case (FloatWidth.F32, IntWidth.I32, Signedness.Unsigned) => opcode(0xb3.toByte)
              case (FloatWidth.F32, IntWidth.I64, Signedness.Signed)   => opcode(0xb4.toByte)
              case (FloatWidth.F32, IntWidth.I64, Signedness.Unsigned) => opcode(0xb5.toByte)
              case (FloatWidth.F64, IntWidth.I32, Signedness.Signed)   => opcode(0xb7.toByte)
              case (FloatWidth.F64, IntWidth.I32, Signedness.Unsigned) => opcode(0xb8.toByte)
              case (FloatWidth.F64, IntWidth.I64, Signedness.Signed)   => opcode(0xb9.toByte)
              case (FloatWidth.F64, IntWidth.I64, Signedness.Unsigned) => opcode(0xba.toByte)
            }
          case Instr.F32DemoteF64                                 => opcode(0xb6.toByte)
          case Instr.F64PromoteF32                                => opcode(0xbb.toByte)
          case Instr.IReinterpretF(intWidth)                      =>
            intWidth match {
              case IntWidth.I32 => opcode(0xbc.toByte)
              case IntWidth.I64 => opcode(0xbd.toByte)
            }
          case Instr.FReinterpretI(floatWidth)                    =>
            floatWidth match {
              case FloatWidth.F32 => opcode(0xbe.toByte)
              case FloatWidth.F64 => opcode(0xbf.toByte)
            }
          case Instr.ITruncSatF(intWidth, floatWidth, signedness) =>
            (intWidth, floatWidth, signedness) match {
              case (IntWidth.I32, FloatWidth.F32, Signedness.Signed)   => opcode(0xfc.toByte) ~> u32.asPrinter(0)
              case (IntWidth.I32, FloatWidth.F32, Signedness.Unsigned) => opcode(0xfc.toByte) ~> u32.asPrinter(1)
              case (IntWidth.I32, FloatWidth.F64, Signedness.Signed)   => opcode(0xfc.toByte) ~> u32.asPrinter(2)
              case (IntWidth.I32, FloatWidth.F64, Signedness.Unsigned) => opcode(0xfc.toByte) ~> u32.asPrinter(3)
              case (IntWidth.I64, FloatWidth.F32, Signedness.Signed)   => opcode(0xfc.toByte) ~> u32.asPrinter(4)
              case (IntWidth.I64, FloatWidth.F32, Signedness.Unsigned) => opcode(0xfc.toByte) ~> u32.asPrinter(5)
              case (IntWidth.I64, FloatWidth.F64, Signedness.Signed)   => opcode(0xfc.toByte) ~> u32.asPrinter(6)
              case (IntWidth.I64, FloatWidth.F64, Signedness.Unsigned) => opcode(0xfc.toByte) ~> u32.asPrinter(7)
            }
          case Instr.V128Const(value)                             => opcode(0xfd.toByte) ~> u32.asPrinter(12) ~> i128.asPrinter(value)
          case Instr.V128Not                                      => opcode(0xfd.toByte) ~> u32.asPrinter(77)
          case Instr.V128And                                      => opcode(0xfd.toByte) ~> u32.asPrinter(78)
          case Instr.V128AndNot                                   => opcode(0xfd.toByte) ~> u32.asPrinter(79)
          case Instr.V128Or                                       => opcode(0xfd.toByte) ~> u32.asPrinter(80)
          case Instr.V128XOr                                      => opcode(0xfd.toByte) ~> u32.asPrinter(81)
          case Instr.V128BitSelect                                => opcode(0xfd.toByte) ~> u32.asPrinter(82)
          case Instr.V128AnyTrue                                  => opcode(0xfd.toByte) ~> u32.asPrinter(83)
          case Instr.VI8x16Shuffle(
                laneIdx0,
                laneIdx1,
                laneIdx2,
                laneIdx3,
                laneIdx4,
                laneIdx5,
                laneIdx6,
                laneIdx7,
                laneIdx8,
                laneIdx9,
                laneIdx10,
                laneIdx11,
                laneIdx12,
                laneIdx13,
                laneIdx14,
                laneIdx15
              ) =>
            opcode(0xfd.toByte) ~> u32.asPrinter(13) ~>
              laneIdx.asPrinter(laneIdx0) ~>
              laneIdx.asPrinter(laneIdx1) ~>
              laneIdx.asPrinter(laneIdx2) ~>
              laneIdx.asPrinter(laneIdx3) ~>
              laneIdx.asPrinter(laneIdx4) ~>
              laneIdx.asPrinter(laneIdx5) ~>
              laneIdx.asPrinter(laneIdx6) ~>
              laneIdx.asPrinter(laneIdx7) ~>
              laneIdx.asPrinter(laneIdx8) ~>
              laneIdx.asPrinter(laneIdx9) ~>
              laneIdx.asPrinter(laneIdx10) ~>
              laneIdx.asPrinter(laneIdx11) ~>
              laneIdx.asPrinter(laneIdx12) ~>
              laneIdx.asPrinter(laneIdx13) ~>
              laneIdx.asPrinter(laneIdx14) ~>
              laneIdx.asPrinter(laneIdx15)
          case Instr.VI18x16Swizzle                               => opcode(0xfd.toByte) ~> u32.asPrinter(14)
          case Instr.VSplat(shape)                                =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 15
                case IShape.I16x8 => 16
                case IShape.I32x4 => 17
                case IShape.I64x2 => 18
                case FShape.F32x4 => 19
                case FShape.F64x2 => 20
              }
            )
          case Instr.VI8x16ExtractLane(signedness, laneIdx)       =>
            opcode(0xfd.toByte) ~>
              u32.asPrinter(signedness match {
                case Signedness.Signed   => 21
                case Signedness.Unsigned => 22
              }) ~>
              Binary.laneIdx.asPrinter(laneIdx)

          case Instr.VI16x8ExtractLane(signedness, laneIdx)    =>
            opcode(0xfd.toByte) ~>
              u32.asPrinter(signedness match {
                case Signedness.Signed   => 24
                case Signedness.Unsigned => 25
              }) ~>
              Binary.laneIdx.asPrinter(laneIdx)
          case Instr.VI32x4ExtractLane(laneIdx)                =>
            opcode(0xfd.toByte) ~> u32.asPrinter(27) ~> Binary.laneIdx.asPrinter(laneIdx)
          case Instr.VI64x2ExtractLane(laneIdx)                =>
            opcode(0xfd.toByte) ~> u32.asPrinter(29) ~> Binary.laneIdx.asPrinter(laneIdx)
          case Instr.VFExtractLane(shape, laneIdx)             =>
            opcode(0xfd.toByte) ~>
              u32.asPrinter(
                shape match {
                  case FShape.F32x4 => 31
                  case FShape.F64x2 => 33
                }
              ) ~>
              Binary.laneIdx.asPrinter(laneIdx)
          case Instr.VReplaceLane(shape, laneIdx)              =>
            opcode(0xfd.toByte) ~>
              u32.asPrinter(
                shape match {
                  case IShape.I8x16 => 23
                  case IShape.I16x8 => 26
                  case IShape.I32x4 => 28
                  case IShape.I64x2 => 30
                  case FShape.F32x4 => 32
                  case FShape.F64x2 => 34
                }
              ) ~>
              Binary.laneIdx.asPrinter(laneIdx)
          case Instr.VIEq(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 35
                case IShape.I16x8 => 45
                case IShape.I32x4 => 55
                case IShape.I64x2 => 214
              }
            )
          case Instr.VINe(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 36
                case IShape.I16x8 => 46
                case IShape.I32x4 => 56
                case IShape.I64x2 => 215
              }
            )
          case Instr.VILt(shape, signedness)                   =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 37
                case (IShape.I8x16, Signedness.Unsigned) => 38
                case (IShape.I16x8, Signedness.Signed)   => 47
                case (IShape.I16x8, Signedness.Unsigned) => 48
                case (IShape.I32x4, Signedness.Signed)   => 57
                case (IShape.I32x4, Signedness.Unsigned) => 58
              }
            )
          case Instr.VIGt(shape, signedness)                   =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 39
                case (IShape.I8x16, Signedness.Unsigned) => 40
                case (IShape.I16x8, Signedness.Signed)   => 49
                case (IShape.I16x8, Signedness.Unsigned) => 50
                case (IShape.I32x4, Signedness.Signed)   => 59
                case (IShape.I32x4, Signedness.Unsigned) => 60
              }
            )
          case Instr.VILe(shape, signedness)                   =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 41
                case (IShape.I8x16, Signedness.Unsigned) => 42
                case (IShape.I16x8, Signedness.Signed)   => 51
                case (IShape.I16x8, Signedness.Unsigned) => 52
                case (IShape.I32x4, Signedness.Signed)   => 61
                case (IShape.I32x4, Signedness.Unsigned) => 62
              }
            )
          case Instr.VIGe(shape, signedness)                   =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 43
                case (IShape.I8x16, Signedness.Unsigned) => 44
                case (IShape.I16x8, Signedness.Signed)   => 53
                case (IShape.I16x8, Signedness.Unsigned) => 54
                case (IShape.I32x4, Signedness.Signed)   => 63
                case (IShape.I32x4, Signedness.Unsigned) => 64
              }
            )
          case Instr.VI64x2Lt                                  => opcode(0xfd.toByte) ~> u32.asPrinter(216)
          case Instr.VI64x2Gt                                  => opcode(0xfd.toByte) ~> u32.asPrinter(217)
          case Instr.VI64x2Le                                  => opcode(0xfd.toByte) ~> u32.asPrinter(218)
          case Instr.VI64x2Ge                                  => opcode(0xfd.toByte) ~> u32.asPrinter(219)
          case Instr.VFEq(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 65
                case FShape.F64x2 => 71
              }
            )
          case Instr.VFNe(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 66
                case FShape.F64x2 => 72
              }
            )
          case Instr.VFLt(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 67
                case FShape.F64x2 => 73
              }
            )
          case Instr.VFGt(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 68
                case FShape.F64x2 => 74
              }
            )
          case Instr.VFLe(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 69
                case FShape.F64x2 => 75
              }
            )
          case Instr.VFGe(shape)                               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 70
                case FShape.F64x2 => 76
              }
            )
          case Instr.VIAbs(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 96
                case IShape.I16x8 => 128
                case IShape.I32x4 => 160
                case IShape.I64x2 => 192
              }
            )
          case Instr.VINeg(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 97
                case IShape.I16x8 => 129
                case IShape.I32x4 => 161
                case IShape.I64x2 => 193
              }
            )
          case Instr.VI8x16PopCnt                              => opcode(0xfd.toByte) ~> u32.asPrinter(98)
          case Instr.VI16x8Q15MulrSat                          => opcode(0xfd.toByte) ~> u32.asPrinter(130)
          case Instr.VI32x4DotI16x8                            => opcode(0xfd.toByte) ~> u32.asPrinter(186)
          case Instr.VFAbs(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 224
                case FShape.F64x2 => 236
              }
            )
          case Instr.VFNeg(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 225
                case FShape.F64x2 => 237
              }
            )
          case Instr.VFSqrt(shape)                             =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 227
                case FShape.F64x2 => 239
              }
            )
          case Instr.VFCeil(shape)                             =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 103
                case FShape.F64x2 => 116
              }
            )
          case Instr.VFFloor(shape)                            =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 104
                case FShape.F64x2 => 117
              }
            )
          case Instr.VFTrunc(shape)                            =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 105
                case FShape.F64x2 => 122
              }
            )
          case Instr.VFNearest(shape)                          =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 106
                case FShape.F64x2 => 148
              }
            )
          case Instr.VIAllTrue(shape)                          =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 99
                case IShape.I16x8 => 131
                case IShape.I32x4 => 163
                case IShape.I64x2 => 195
              }
            )
          case Instr.VIBitMask(shape)                          =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 100
                case IShape.I16x8 => 132
                case IShape.I32x4 => 164
                case IShape.I64x2 => 196
              }
            )
          case Instr.VI8x16NarrowI16x8(signedness)             =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              signedness match {
                case Signedness.Signed   => 101
                case Signedness.Unsigned => 102
              }
            )
          case Instr.VI16x8NarrowI32x4(signedness)             =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              signedness match {
                case Signedness.Signed   => 133
                case Signedness.Unsigned => 134
              }
            )
          case Instr.VI16x8ExtendI8x16(half, signedness)       =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (half, signedness) match {
                case (Half.Low, Signedness.Signed)    => 135
                case (Half.High, Signedness.Signed)   => 136
                case (Half.Low, Signedness.Unsigned)  => 137
                case (Half.High, Signedness.Unsigned) => 138
              }
            )
          case Instr.VI32x4ExtendI16x8(half, signedness)       =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (half, signedness) match {
                case (Half.Low, Signedness.Signed)    => 167
                case (Half.High, Signedness.Signed)   => 168
                case (Half.Low, Signedness.Unsigned)  => 169
                case (Half.High, Signedness.Unsigned) => 170
              }
            )
          case Instr.VI64x2ExtendI32x4(half, signedness)       =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (half, signedness) match {
                case (Half.Low, Signedness.Signed)    => 199
                case (Half.High, Signedness.Signed)   => 200
                case (Half.Low, Signedness.Unsigned)  => 201
                case (Half.High, Signedness.Unsigned) => 202
              }
            )
          case Instr.VIShl(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 107
                case IShape.I16x8 => 139
                case IShape.I32x4 => 171
                case IShape.I64x2 => 203
              }
            )
          case Instr.VIShr(shape, signedness)                  =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 108
                case (IShape.I8x16, Signedness.Unsigned) => 109
                case (IShape.I16x8, Signedness.Signed)   => 140
                case (IShape.I16x8, Signedness.Unsigned) => 141
                case (IShape.I32x4, Signedness.Signed)   => 172
                case (IShape.I32x4, Signedness.Unsigned) => 173
                case (IShape.I64x2, Signedness.Signed)   => 204
                case (IShape.I64x2, Signedness.Unsigned) => 205
              }
            )
          case Instr.VIAdd(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 110
                case IShape.I16x8 => 142
                case IShape.I32x4 => 174
                case IShape.I64x2 => 206
              }
            )
          case Instr.VISub(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 113
                case IShape.I16x8 => 145
                case IShape.I32x4 => 177
                case IShape.I64x2 => 209
              }
            )
          case Instr.VIMin(shape, signedness)                  =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 118
                case (IShape.I8x16, Signedness.Unsigned) => 119
                case (IShape.I16x8, Signedness.Signed)   => 150
                case (IShape.I16x8, Signedness.Unsigned) => 151
                case (IShape.I32x4, Signedness.Signed)   => 182
                case (IShape.I32x4, Signedness.Unsigned) => 183
              }
            )
          case Instr.VIMax(shape, signedness)                  =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 120
                case (IShape.I8x16, Signedness.Unsigned) => 121
                case (IShape.I16x8, Signedness.Signed)   => 152
                case (IShape.I16x8, Signedness.Unsigned) => 153
                case (IShape.I32x4, Signedness.Signed)   => 184
                case (IShape.I32x4, Signedness.Unsigned) => 185
              }
            )
          case Instr.VIAddSat(shape, signedness)               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 111
                case (IShape.I8x16, Signedness.Unsigned) => 112
                case (IShape.I16x8, Signedness.Signed)   => 143
                case (IShape.I16x8, Signedness.Unsigned) => 144
              }
            )
          case Instr.VISubSat(shape, signedness)               =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I8x16, Signedness.Signed)   => 114
                case (IShape.I8x16, Signedness.Unsigned) => 115
                case (IShape.I16x8, Signedness.Signed)   => 146
                case (IShape.I16x8, Signedness.Unsigned) => 147
              }
            )
          case Instr.VIMul(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I16x8 => 149
                case IShape.I32x4 => 181
                case IShape.I64x2 => 213
              }
            )
          case Instr.VIAvgr(shape)                             =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case IShape.I8x16 => 123
                case IShape.I16x8 => 155
              }
            )
          case Instr.VIExtMul(shape, half, signedness)         =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, half, signedness) match {
                case (IShape.I16x8, Half.Low, Signedness.Signed)    => 156
                case (IShape.I16x8, Half.High, Signedness.Signed)   => 157
                case (IShape.I16x8, Half.Low, Signedness.Unsigned)  => 158
                case (IShape.I16x8, Half.High, Signedness.Unsigned) => 159
                case (IShape.I32x4, Half.Low, Signedness.Signed)    => 188
                case (IShape.I32x4, Half.High, Signedness.Signed)   => 189
                case (IShape.I32x4, Half.Low, Signedness.Unsigned)  => 190
                case (IShape.I32x4, Half.High, Signedness.Unsigned) => 191
                case (IShape.I64x2, Half.Low, Signedness.Signed)    => 220
                case (IShape.I64x2, Half.High, Signedness.Signed)   => 221
                case (IShape.I64x2, Half.Low, Signedness.Unsigned)  => 222
                case (IShape.I64x2, Half.High, Signedness.Unsigned) => 223
              }
            )
          case Instr.VIExtAddPairwise(shape, signedness)       =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              (shape, signedness) match {
                case (IShape.I16x8, Signedness.Signed)   => 124
                case (IShape.I16x8, Signedness.Unsigned) => 125
                case (IShape.I32x4, Signedness.Signed)   => 126
                case (IShape.I32x4, Signedness.Unsigned) => 127
              }
            )
          case Instr.VFAdd(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 228
                case FShape.F64x2 => 240
              }
            )
          case Instr.VFSub(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 229
                case FShape.F64x2 => 241
              }
            )
          case Instr.VFMul(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 230
                case FShape.F64x2 => 242
              }
            )
          case Instr.VFDiv(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 231
                case FShape.F64x2 => 243
              }
            )
          case Instr.VFMin(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 232
                case FShape.F64x2 => 244
              }
            )
          case Instr.VFMax(shape)                              =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 233
                case FShape.F64x2 => 245
              }
            )
          case Instr.VFPMin(shape)                             =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 234
                case FShape.F64x2 => 246
              }
            )
          case Instr.VFPMax(shape)                             =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              shape match {
                case FShape.F32x4 => 235
                case FShape.F64x2 => 247
              }
            )
          case Instr.VI32x4TruncSatF32x4(signedness)           =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              signedness match {
                case Signedness.Signed   => 248
                case Signedness.Unsigned => 249
              }
            )
          case Instr.VI32x4TruncSatF64x2Zero(signedness)       =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              signedness match {
                case Signedness.Signed   => 252
                case Signedness.Unsigned => 253
              }
            )
          case Instr.VI32x4ConvertI32x4(signedness)            =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              signedness match {
                case Signedness.Signed   => 250
                case Signedness.Unsigned => 251
              }
            )
          case Instr.VF32x4DemoteF64x2Zero                     => opcode(0xfd.toByte) ~> u32.asPrinter(94)
          case Instr.VF64x2ConvertLowI32x4(signedness)         =>
            opcode(0xfd.toByte) ~> u32.asPrinter(
              signedness match {
                case Signedness.Signed   => 254
                case Signedness.Unsigned => 255
              }
            )
          case Instr.VF64x2PromoteLowI32x4                     => opcode(0xfd.toByte) ~> u32.asPrinter(95)
          case Instr.RefNull(refType)                          => opcode(0xd0.toByte) ~> Binary.refType.asPrinter(refType)
          case Instr.RefIsNull                                 => opcode(0xd1.toByte)
          case Instr.RefFunc(idx)                              => opcode(0xd2.toByte) ~> Binary.funcIdx.asPrinter(idx)
          case Instr.Drop                                      => opcode(0x1a)
          case Instr.Select(types)                             =>
            types match {
              case Some(types) => opcode(0x1c) ~> vec(Binary.valType).asPrinter(types)
              case None        => opcode(0x1b)
            }
          case Instr.LocalGet(idx)                             => opcode(0x20) ~> Binary.localIdx.asPrinter(idx)
          case Instr.LocalSet(idx)                             => opcode(0x21) ~> Binary.localIdx.asPrinter(idx)
          case Instr.LocalTee(idx)                             => opcode(0x22) ~> Binary.localIdx.asPrinter(idx)
          case Instr.GlobalGet(idx)                            => opcode(0x23) ~> Binary.globalIdx.asPrinter(idx)
          case Instr.GlobalSet(idx)                            => opcode(0x24) ~> Binary.globalIdx.asPrinter(idx)
          case Instr.TableGet(idx)                             => opcode(0x25) ~> Binary.tableIdx.asPrinter(idx)
          case Instr.TableSet(idx)                             => opcode(0x26) ~> Binary.tableIdx.asPrinter(idx)
          case Instr.TableSize(idx)                            => opcode(0xfc.toByte) ~> u32.asPrinter(16) ~> Binary.tableIdx.asPrinter(idx)
          case Instr.TableGrow(idx)                            => opcode(0xfc.toByte) ~> u32.asPrinter(15) ~> Binary.tableIdx.asPrinter(idx)
          case Instr.TableFill(idx)                            => opcode(0xfc.toByte) ~> u32.asPrinter(17) ~> Binary.tableIdx.asPrinter(idx)
          case Instr.TableCopy(source, destination)            =>
            opcode(0xfc.toByte) ~> u32.asPrinter(14) ~> Binary.tableIdx.asPrinter(source) ~> Binary.tableIdx.asPrinter(
              destination
            )
          case Instr.TableInit(tableIdx, elemIdx)              =>
            opcode(0xfc.toByte) ~> u32.asPrinter(12) ~> Binary.tableIdx.asPrinter(tableIdx) ~> Binary.elemIdx.asPrinter(
              elemIdx
            )
          case Instr.ElemDrop(idx)                             => opcode(0xfc.toByte) ~> u32.asPrinter(13) ~> Binary.elemIdx.asPrinter(idx)
          case Instr.Load(typ, memArg)                         =>
            (typ match {
              case NumType.I32  => opcode(0x28)
              case NumType.I64  => opcode(0x29)
              case NumType.F32  => opcode(0x2a)
              case NumType.F64  => opcode(0x2b)
              case VecType.V128 => opcode(0xfd.toByte) ~> u32.asPrinter(0)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.Store(typ, memArg)                        =>
            (typ match {
              case NumType.I32  => opcode(0x36)
              case NumType.I64  => opcode(0x37)
              case NumType.F32  => opcode(0x38)
              case NumType.F64  => opcode(0x39)
              case VecType.V128 => opcode(0xfd.toByte) ~> u32.asPrinter(11)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.Load8(typ, signedness, memArg)            =>
            ((typ, signedness) match {
              case (NumType.I32, Signedness.Signed)   => opcode(0x2c)
              case (NumType.I32, Signedness.Unsigned) => opcode(0x2d)
              case (NumType.I64, Signedness.Signed)   => opcode(0x30)
              case (NumType.I64, Signedness.Unsigned) => opcode(0x31)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.Load16(typ, signedness, memArg)           =>
            ((typ, signedness) match {
              case (NumType.I32, Signedness.Signed)   => opcode(0x2e)
              case (NumType.I32, Signedness.Unsigned) => opcode(0x2f)
              case (NumType.I64, Signedness.Signed)   => opcode(0x32)
              case (NumType.I64, Signedness.Unsigned) => opcode(0x33)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.Load32(signedness, memArg)                =>
            (signedness match {
              case Signedness.Signed   => opcode(0x34)
              case Signedness.Unsigned => opcode(0x35)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.Store8(typ, memArg)                       =>
            (typ match {
              case NumType.I32 => opcode(0x3a)
              case NumType.I64 => opcode(0x3c)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.Store16(typ, memArg)                      =>
            (typ match {
              case NumType.I32 => opcode(0x3b)
              case NumType.I64 => opcode(0x3d)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.Store32(memArg)                           =>
            opcode(0x3e) ~> Binary.memArg.asPrinter(memArg)
          case Instr.V128Load8x8(signedness, memArg)           =>
            (signedness match {
              case Signedness.Signed   => opcode(0xfd.toByte) ~> u32.asPrinter(1)
              case Signedness.Unsigned => opcode(0xfd.toByte) ~> u32.asPrinter(2)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.V128Load16x4(signedness, memArg)          =>
            (signedness match {
              case Signedness.Signed   => opcode(0xfd.toByte) ~> u32.asPrinter(3)
              case Signedness.Unsigned => opcode(0xfd.toByte) ~> u32.asPrinter(4)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.V128Load32x2(signedness, memArg)          =>
            (signedness match {
              case Signedness.Signed   => opcode(0xfd.toByte) ~> u32.asPrinter(5)
              case Signedness.Unsigned => opcode(0xfd.toByte) ~> u32.asPrinter(6)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.V128Load32Zero(memArg)                    =>
            opcode(0xfd.toByte) ~> u32.asPrinter(92) ~> Binary.memArg.asPrinter(memArg)
          case Instr.V128Load64Zero(memArg)                    =>
            opcode(0xfd.toByte) ~> u32.asPrinter(93) ~> Binary.memArg.asPrinter(memArg)
          case Instr.V128LoadSplat(loadShape, memArg)          =>
            (loadShape match {
              case VectorLoadShape.WW8  => opcode(0xfd.toByte) ~> u32.asPrinter(7)
              case VectorLoadShape.WW16 => opcode(0xfd.toByte) ~> u32.asPrinter(8)
              case VectorLoadShape.WW32 => opcode(0xfd.toByte) ~> u32.asPrinter(9)
              case VectorLoadShape.WW64 => opcode(0xfd.toByte) ~> u32.asPrinter(10)
            }) ~> Binary.memArg.asPrinter(memArg)
          case Instr.V128LoadLane(loadShape, memArg, laneIdx)  =>
            (loadShape match {
              case VectorLoadShape.WW8  => opcode(0xfd.toByte) ~> u32.asPrinter(84)
              case VectorLoadShape.WW16 => opcode(0xfd.toByte) ~> u32.asPrinter(85)
              case VectorLoadShape.WW32 => opcode(0xfd.toByte) ~> u32.asPrinter(86)
              case VectorLoadShape.WW64 => opcode(0xfd.toByte) ~> u32.asPrinter(87)
            }) ~> Binary.memArg.asPrinter(memArg) ~> Binary.laneIdx.asPrinter(laneIdx)
          case Instr.V128StoreLane(loadShape, memArg, laneIdx) =>
            (loadShape match {
              case VectorLoadShape.WW8  => opcode(0xfd.toByte) ~> u32.asPrinter(88)
              case VectorLoadShape.WW16 => opcode(0xfd.toByte) ~> u32.asPrinter(89)
              case VectorLoadShape.WW32 => opcode(0xfd.toByte) ~> u32.asPrinter(90)
              case VectorLoadShape.WW64 => opcode(0xfd.toByte) ~> u32.asPrinter(91)
            }) ~> Binary.memArg.asPrinter(memArg) ~> Binary.laneIdx.asPrinter(laneIdx)
          case Instr.MemorySize                                => opcode(0x3f.toByte) ~> opcode(0x00)
          case Instr.MemoryGrow                                => opcode(0x40.toByte) ~> opcode(0x00)
          case Instr.MemoryFill                                => opcode(0xfc.toByte) ~> u32.asPrinter(11) ~> opcode(0x00)
          case Instr.MemoryCopy                                => opcode(0xfc.toByte) ~> u32.asPrinter(10) ~> opcode(0x00) ~> opcode(0x00)
          case Instr.MemoryInit(idx)                           =>
            opcode(0xfc.toByte) ~> u32.asPrinter(8) ~> Binary.dataIdx.asPrinter(idx) ~> opcode(0x00)
          case Instr.DataDrop(idx)                             => opcode(0xfc.toByte) ~> u32.asPrinter(9) ~> Binary.dataIdx.asPrinter(idx)
          case Instr.Nop                                       => opcode(0x01)
          case Instr.Unreachable                               => opcode(0x00)
          case Instr.Block(blockType, instructions)            =>
            opcode(0x02) ~>
              Binary.blockType.asPrinter(blockType) ~>
              instructionSequence(instructions, 0x0b.toByte)
          case Instr.Loop(blockType, instructions)             =>
            opcode(0x03) ~>
              Binary.blockType.asPrinter(blockType) ~>
              instructionSequence(instructions, 0x0b.toByte)
          case Instr.If(blockType, trueInstrs, falseInstrs)    =>
            if (falseInstrs.isEmpty) {
              opcode(0x04) ~>
                Binary.blockType.asPrinter(blockType) ~>
                instructionSequence(trueInstrs, 0x0b.toByte)
            } else {
              opcode(0x04) ~>
                Binary.blockType.asPrinter(blockType) ~>
                instructionSequence(trueInstrs, 0x05.toByte) ~>
                instructionSequence(falseInstrs, 0x0b.toByte)
            }
          case Instr.Br(labelIdx)                              => opcode(0x0c) ~> Binary.labelIdx.asPrinter(labelIdx)
          case Instr.BrIf(labelIdx)                            => opcode(0x0d) ~> Binary.labelIdx.asPrinter(labelIdx)
          case Instr.BrTable(labels, default)                  =>
            opcode(0x0e) ~> vec(Binary.labelIdx).asPrinter(labels) ~> Binary.labelIdx.asPrinter(default)
          case Instr.Return                                    => opcode(0x0f)
          case Instr.Call(idx)                                 => opcode(0x10) ~> Binary.funcIdx.asPrinter(idx)
          case Instr.CallIndirect(tableIdx, typeIdx)           =>
            opcode(0x11) ~> Binary.typeIdx.asPrinter(typeIdx) ~> Binary.tableIdx.asPrinter(tableIdx)
        }
      }

    private def opcode(code: Byte): Printer[Nothing, Byte, Any] = Printer.print(code)

    private def instructionSequence(instructions: Chunk[Instr], terminatedBy: Byte) =
      Binary.instr.repeatUntil(specificByte_(terminatedBy)).asPrinter(instructions)
  }

  private[wasm] val instr: BinarySyntax[Instr] = (Instructions.read <=> Instructions.write) ?? "instr"

  private[wasm] val expr: BinarySyntax[Expr] =
    instr.repeatUntil(specificByte_(0x0b.toByte).backtrack).of[Expr] ?? "expr"

  private[wasm] opaque type SectionId = Byte
  private[wasm] object SectionId {
    def fromByte(byte: Byte): SectionId = byte
  }

  private[wasm] final case class RawSection(id: SectionId, size: Int, rawContents: Chunk[Byte]) {
    def to[T](syntax: BinarySyntax[T]): Either[SyntaxError, T] =
//      println(s"Parsing section of type $id, length $size\n${rawContents.map(_.toInt.toHexString).mkString(" ")}")
//      println(s"Parsing section of type $id, length $size\n")
      (syntax)
        .parseChunk(rawContents)
        .left
        .map { err =>
          SyntaxError.InnerParserError(err)
        }
        .map { v =>
//          println(s" => $v")
          v
        }
  }

  private[wasm] object RawSection {
    val custom: SectionId    = 0
    val `type`: SectionId    = 1
    val `import`: SectionId  = 2
    val function: SectionId  = 3
    val table: SectionId     = 4
    val memory: SectionId    = 5
    val global: SectionId    = 6
    val `export`: SectionId  = 7
    val start: SectionId     = 8
    val element: SectionId   = 9
    val code: SectionId      = 10
    val data: SectionId      = 11
    val dataCount: SectionId = 12

    def of[T](id: SectionId, syntax: BinarySyntax[T], value: T): Either[SyntaxError, RawSection] =
      syntax.print(value).map { rawContents =>
        RawSection(id, rawContents.size, rawContents)
      }
  }

  private[wasm] val section: BinarySyntax[RawSection] = {
    val parser  = (anyByte ~ u32).asParser.flatMap { case (id, size) =>
      anyByte.asParser.exactly(size).map { rawContents =>
        RawSection(id, size, rawContents)
      }
    }
    val printer = Printer.byValue { (section: RawSection) =>
      Printer.print(section.id) ~>
        u32.asPrinter(section.size) ~>
        anyBytes.asPrinter(section.rawContents)
    }

    parser <=> printer
  }

  private[wasm] val magic =
    specificByte_(0x00) ~>
      specificByte_(0x61) ~>
      specificByte_(0x73) ~>
      specificByte_(0x6d)

  private val version =
    specificByte_(0x01) ~>
      specificByte_(0x00) ~>
      specificByte_(0x00) ~>
      specificByte_(0x00)

  private val typeSection: BinarySyntax[Chunk[FuncType]] = vec(funcType) ?? "typeSection"

  private val importDesc: BinarySyntax[ImportDesc] =
    ((specificByte_(0x00) ~> funcIdx).transformTo(
      ImportDesc.Func.apply,
      { case ImportDesc.Func(funcIdx) => funcIdx },
      SyntaxError.InvalidImportDesc
    ) <>
      (specificByte_(0x01) ~> tableType).transformTo(
        ImportDesc.Table.apply,
        { case ImportDesc.Table(tableType) => tableType },
        SyntaxError.InvalidImportDesc
      ) <>
      (specificByte_(0x02) ~> memoryType).transformTo(
        ImportDesc.Mem.apply,
        { case ImportDesc.Mem(memType) => memType },
        SyntaxError.InvalidImportDesc
      ) <>
      (specificByte_(0x03) ~> globalType).transformTo(
        ImportDesc.Global.apply,
        { case ImportDesc.Global(globalType) => globalType },
        SyntaxError.InvalidImportDesc
      )) ?? "importDesc"

  private[wasm] val `import`: BinarySyntax[Import]       = (name ~ name ~ importDesc).of[Import] ?? "import"
  private val importSection: BinarySyntax[Chunk[Import]] = vec(`import`) ?? "importSection"

  private val functionSection: BinarySyntax[Chunk[FuncTypeRef]] = vec(typeIdx.of[FuncTypeRef]) ?? "functionSection"

  private val table: BinarySyntax[Table]               = tableType.of[Table] ?? "table"
  private val tableSection: BinarySyntax[Chunk[Table]] = vec(table) ?? "tableSection"

  private val mem                                     = memoryType.of[Mem] ?? "mem"
  private val memorySection: BinarySyntax[Chunk[Mem]] = vec(mem) ?? "memorySection"

  private val global: BinarySyntax[Global]               = (globalType ~ expr).of[Global] ?? "global"
  private val globalSection: BinarySyntax[Chunk[Global]] = vec(global) ?? "globalSection"

  private[wasm] val exportDesc: BinarySyntax[ExportDesc] =
    ((specificByte_(0x00) ~> funcIdx).transformTo(
      ExportDesc.Func.apply,
      { case ExportDesc.Func(funcIdx) => funcIdx },
      SyntaxError.InvalidExportDesc
    ) <>
      (specificByte_(0x01) ~> tableIdx).transformTo(
        ExportDesc.Table.apply,
        { case ExportDesc.Table(tableIdx) => tableIdx },
        SyntaxError.InvalidExportDesc
      ) <>
      (specificByte_(0x02) ~> memIdx).transformTo(
        ExportDesc.Mem.apply,
        { case ExportDesc.Mem(memIdx) => memIdx },
        SyntaxError.InvalidExportDesc
      ) <>
      (specificByte_(0x03) ~> globalIdx).transformTo(
        ExportDesc.Global.apply,
        { case ExportDesc.Global(globalIdx) => globalIdx },
        SyntaxError.InvalidExportDesc
      )) ?? "exportDesc"
  private[wasm] val `export`: BinarySyntax[Export]       = (name ~ exportDesc).of[Export] ?? "export"
  private val exportSection: BinarySyntax[Chunk[Export]] = vec(`export`) ?? "exportSection"

  private val start: BinarySyntax[Start]        = funcIdx.of[Start] ?? "start"
  private val startSection: BinarySyntax[Start] = start ?? "startSection"

  private val elemKind: BinarySyntax[RefType] =
    anyByte.transformEither[SyntaxError, RefType](
      {
        case 0x00 => Right(RefType.FuncRef)
        case _    => Left(SyntaxError.InvalidElemKind)
      },
      {
        case RefType.FuncRef => Right(0x00)
        case _               => Left(SyntaxError.InvalidElemKind)
      }
    ) ?? "elemKind"

  private val elem: BinarySyntax[Elem]               = {
    val parser = u32.asParser.flatMap {
      case 0 =>
        (expr ~ vec(funcIdx)).asParser.map { case (e, ys) =>
          Elem(RefType.FuncRef, ys.map(y => Expr(Chunk(Instr.RefFunc(y)))), ElemMode.Active(TableIdx.fromInt(0), e))
        }
      case 1 =>
        (elemKind ~ vec(funcIdx)).asParser.map { case (et, ys) =>
          Elem(et, ys.map(y => Expr(Chunk(Instr.RefFunc(y)))), ElemMode.Passive)
        }
      case 2 =>
        (tableIdx ~ expr ~ elemKind ~ vec(funcIdx)).asParser.map { case (x, e, et, ys) =>
          Elem(et, ys.map(y => Expr(Chunk(Instr.RefFunc(y)))), ElemMode.Active(x, e))
        }
      case 3 =>
        (elemKind ~ vec(funcIdx)).asParser.map { case (et, ys) =>
          Elem(et, ys.map(y => Expr(Chunk(Instr.RefFunc(y)))), ElemMode.Declarative)
        }
      case 4 =>
        (expr ~ vec(expr)).asParser.map { case (e, els) =>
          Elem(RefType.FuncRef, els, ElemMode.Active(TableIdx.fromInt(0), e))
        }
      case 5 =>
        (refType ~ vec(expr)).asParser.map { case (et, els) =>
          Elem(et, els, ElemMode.Passive)
        }
      case 6 =>
        (tableIdx ~ expr ~ refType ~ vec(expr)).asParser.map { case (x, e, et, els) =>
          Elem(et, els, ElemMode.Active(x, e))
        }
      case 7 =>
        (refType ~ vec(expr)).asParser.map { case (et, els) =>
          Elem(et, els, ElemMode.Declarative)
        }
    }

    val printer = Printer.byValue { (elem: Elem) =>
      elem match {
        case Elem(RefType.FuncRef, ys, ElemMode.Active(t, e)) if t == TableIdx.fromInt(0) =>
          val funcIdxs = ys.collect { case Expr(Chunk(Instr.RefFunc(y))) =>
            y
          }
          if (funcIdxs.size == ys.size) {
            u32.asPrinter(0) ~> expr.asPrinter(e) ~ vec(funcIdx).asPrinter(funcIdxs)
          } else {
            u32.asPrinter(4) ~> expr.asPrinter(e) ~ vec(expr).asPrinter(ys)
          }
        case Elem(et, ys, ElemMode.Passive)                                               =>
          val funcIdxs = ys.collect { case Expr(Chunk(Instr.RefFunc(y))) =>
            y
          }
          if (funcIdxs.size == ys.size) {
            u32.asPrinter(1) ~> elemKind.asPrinter(et) ~> vec(funcIdx).asPrinter(funcIdxs)
          } else {
            u32.asPrinter(5) ~> refType.asPrinter(et) ~> vec(expr).asPrinter(ys)
          }
        case Elem(et, ys, ElemMode.Active(t, e))                                          =>
          val funcIdxs = ys.collect { case Expr(Chunk(Instr.RefFunc(y))) =>
            y
          }
          if (funcIdxs.size == ys.size) {
            u32.asPrinter(2) ~> tableIdx.asPrinter(t) ~> expr.asPrinter(e) ~> elemKind.asPrinter(et) ~>
              vec(funcIdx).asPrinter(funcIdxs)
          } else {
            u32.asPrinter(6) ~> tableIdx.asPrinter(t) ~> expr.asPrinter(e) ~> refType.asPrinter(et) ~>
              vec(expr).asPrinter(ys)
          }
        case Elem(et, ys, ElemMode.Declarative)                                           =>
          val funcIdxs = ys.collect { case Expr(Chunk(Instr.RefFunc(y))) =>
            y
          }
          if (funcIdxs.size == ys.size) {
            u32.asPrinter(3) ~> elemKind.asPrinter(et) ~> vec(funcIdx).asPrinter(funcIdxs)
          } else {
            u32.asPrinter(7) ~> refType.asPrinter(et) ~> vec(expr).asPrinter(ys)
          }
      }
    }

    (parser <=> printer) ?? "elem"
  }
  private val elemSection: BinarySyntax[Chunk[Elem]] = vec(elem) ?? "elemSection"

  private val locals: BinarySyntax[(Int, ValType)]              = (u32 ~ valType) ?? "locals"
  private val func: BinarySyntax[(Chunk[(Int, ValType)], Expr)] =
    (vec(locals) ~ expr) ?? "func"
  private val code: BinarySyntax[FuncCode]                      = {
    val parser  = (u32.unit(0) ~> func).asParser
    val printer = Printer.byValue { (value: (Chunk[(Int, ValType)], Expr)) =>
      func.print(value) match {
        case Left(error) => Printer.fail(error)
        case Right(raw)  => u32.asPrinter(raw.size) ~> anyBytes.asPrinter(raw)
      }
    }

    (parser <=> printer).transform(
      { case (locals, body) => FuncCode(uncompressLocals(locals), body) },
      (code: FuncCode) => (compressLocals(code.locals), code.body)
    ) ?? "code"
  }
  private val codeSection: BinarySyntax[Chunk[FuncCode]]        = vec(code) ?? "codeSection"

  private val data: BinarySyntax[Data]               = {
    val parser = u32.asParser.flatMap {
      case 0 =>
        (expr ~ vec(anyByte)).asParser.map { case (e, bs) =>
          Data(bs, DataMode.Active(MemIdx.fromInt(0), e))
        }
      case 1 =>
        (vec(anyByte)).asParser.map { bs =>
          Data(bs, DataMode.Passive)
        }
      case 2 =>
        (memIdx ~ expr ~ vec(anyByte)).asParser.map { case (x, e, bs) =>
          Data(bs, DataMode.Active(x, e))
        }
    }

    val printer = Printer.byValue { (data: Data) =>
      data match {
        case Data(bs, DataMode.Active(m, e)) if m == MemIdx.fromInt(0) =>
          u32.asPrinter(0) ~> expr.asPrinter(e) ~> vec(anyByte).asPrinter(bs)
        case Data(bs, DataMode.Passive)                                =>
          u32.asPrinter(1) ~> vec(anyByte).asPrinter(bs)
        case Data(bs, DataMode.Active(m, e))                           =>
          u32.asPrinter(2) ~> memIdx.asPrinter(m) ~> expr.asPrinter(e) ~> vec(anyByte).asPrinter(bs)
      }
    }

    (parser <=> printer) ?? "data"
  }
  private val dataSection: BinarySyntax[Chunk[Data]] = vec(data) ?? "dataSection"

  private val dataCountSection: BinarySyntax[Int] = u32 ?? "dataCountSection"

  private def compressLocals(locals: Chunk[ValType]): Chunk[(Int, ValType)] = {
    val builder = ChunkBuilder.make[(Int, ValType)](locals.size)
    if (locals.isEmpty) Chunk.empty
    else {
      var last: ValType = locals.head
      var count: Int    = 1
      for (i <- 1 until locals.size) {
        val current = locals(i)
        if (current == last) {
          count += 1
        } else {
          builder += ((count, last))
          count = 1
          last = current
        }
      }
      builder += ((count, last))
      builder.result()
    }
  }

  private def uncompressLocals(locals: Chunk[(Int, ValType)]): Chunk[ValType] =
    locals.flatMap { case (n, vt) => Chunk.fill(n)(vt) }

  private def fromSections(sections: Chunk[RawSection]): Either[SyntaxError, Module] = {
    def loadSection(section: RawSection): Either[SyntaxError, Chunk[Section[CoreIndexSpace]]] =
      section.id match {
        case RawSection.`type`    => section.to(typeSection)
        case RawSection.`import`  => section.to(importSection)
        case RawSection.function  => section.to(functionSection)
        case RawSection.code      => section.to(codeSection)
        case RawSection.table     => section.to(tableSection)
        case RawSection.memory    => section.to(memorySection)
        case RawSection.global    => section.to(globalSection)
        case RawSection.element   => section.to(elemSection)
        case RawSection.data      => section.to(dataSection)
        case RawSection.start     => section.to(startSection).map(Chunk.single)
        case RawSection.`export`  => section.to(exportSection)
        case RawSection.dataCount => Right(Chunk.empty) // skipping
        case _                    => section.to(name ~ anyBytes).map { case (n, bs) => Chunk(Custom(n, bs)) }
      }

    sections.forEach(loadSection).map { sections =>
      Module(Sections.fromGrouped(sections.filter(_.nonEmpty)))
    }
  }

  private def toSections(module: Module): Either[SyntaxError, Chunk[RawSection]] = {
    def encodeGroup(
        sectionType: SectionType[CoreIndexSpace],
        sections: Chunk[Section[CoreIndexSpace]]
    ): Either[SyntaxError, RawSection] =
      sectionType match {
        case SectionType.CoreTypeSection      =>
          RawSection.of(sectionType.binaryId, typeSection, sections.asInstanceOf[Chunk[FuncType]])
        case SectionType.CoreImportSection    =>
          RawSection.of(sectionType.binaryId, importSection, sections.asInstanceOf[Chunk[Import]])
        case SectionType.CoreFuncSection      =>
          RawSection.of(sectionType.binaryId, functionSection, sections.asInstanceOf[Chunk[FuncTypeRef]])
        case SectionType.CoreTableSection     =>
          RawSection.of(sectionType.binaryId, tableSection, sections.asInstanceOf[Chunk[Table]])
        case SectionType.CoreMemSection       =>
          RawSection.of(sectionType.binaryId, memorySection, sections.asInstanceOf[Chunk[Mem]])
        case SectionType.CoreGlobalSection    =>
          RawSection.of(sectionType.binaryId, globalSection, sections.asInstanceOf[Chunk[Global]])
        case SectionType.CoreExportSection    =>
          RawSection.of(sectionType.binaryId, exportSection, sections.asInstanceOf[Chunk[Export]])
        case SectionType.CoreStartSection     =>
          RawSection.of(sectionType.binaryId, startSection, sections.head.asInstanceOf[Start])
        case SectionType.CoreElemSection      =>
          RawSection.of(sectionType.binaryId, elemSection, sections.asInstanceOf[Chunk[Elem]])
        case SectionType.CoreDataSection      =>
          RawSection.of(sectionType.binaryId, dataSection, sections.asInstanceOf[Chunk[Data]])
        case SectionType.CoreCodeSection      =>
          RawSection.of(sectionType.binaryId, codeSection, sections.asInstanceOf[Chunk[FuncCode]])
        case SectionType.CoreDataCountSection =>
          RawSection.of(sectionType.binaryId, dataCountSection, sections.size)
        case SectionType.CustomSection        =>
          val custom = sections.head.asInstanceOf[Custom]
          name.asPrinter.print(custom.name).map { nameBytes =>
            val data = nameBytes ++ custom.data
            RawSection(RawSection.custom, data.size, data)
          }
      }

    module.sections.toGrouped.forEach { case (sectionType, sections) => encodeGroup(sectionType, sections) }
  }

  private val moduleValue: BinarySyntax[Module] =
    section.repeat0.transformEither(fromSections, toSections) ?? "module"

  val module: BinarySyntax[Module] =
    (magic ~> version ~> moduleValue) ?? "module file"
}
