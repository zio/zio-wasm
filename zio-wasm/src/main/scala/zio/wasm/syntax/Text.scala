package zio.wasm.syntax

import zio.{Chunk, ChunkBuilder, NonEmptyChunk}
import zio.parser.*
import zio.wasm.{LabelIdx, *}
import zio.wasm.syntax.Text.*

import scala.reflect.ClassTag

class Text(using ctx: IdentifierContext) {
  // TODO: clean up errors

  private[wasm] val char   = Syntax.anyChar
  private[wasm] val source = char.*

  private[wasm] val format: TextSyntax[Unit] =
    Syntax.charIn('\n', '\r', '\t').mapError(_ => SyntaxError.UnexpectedByte).unit(' ') ?? "format"

  private[wasm] val linechar                       = Syntax.charNotIn('\n') ?? "linechar"
  private[wasm] val linecomment: TextSyntax[Unit]  =
    (Syntax.string(";;", ()) ~> linechar.* ~ (Syntax.end | Syntax.char('\n')))
      .unit(Chunk.empty)
      .mapError(_ => SyntaxError.InvalidComment) ?? "linecomment"
  private[wasm] val blockchar                      = Syntax.charNotIn(';', ')') ?? "blockchar"
  private[wasm] val blockcomment: TextSyntax[Unit] =
    (Syntax.string("(;", ()) ~> blockchar.* ~ Syntax.string(";)", ()))
      .unit(Chunk.empty)
      .mapError(_ => SyntaxError.InvalidComment) ?? "blockcomment"
  private[wasm] val comment: TextSyntax[Unit]      = linecomment | blockcomment

  private[wasm] val space: TextSyntax[Unit] =
    (Syntax.char(' ').mapError(_ => SyntaxError.UnexpectedByte) |
      format |
      comment).*.unit(Chunk.single(())) ?? "space"

  private[wasm] val space0: TextSyntax[Unit] = space.asParser <=> Printer.unit

  private def vec[A](inner: TextSyntax[A]): TextSyntax[Chunk[A]] = inner.repeatWithSep0(space)

  private val lpar                                    = Syntax.char('(').mapError(_ => SyntaxError.UnexpectedByte) ?? "lpar"
  private val rpar                                    = Syntax.char(')').mapError(_ => SyntaxError.UnexpectedByte) ?? "rpar"
  private def keyword(name: String): TextSyntax[Unit] =
    Syntax.string(name, ()).mapError(SyntaxError.UnexpectedKeyword.apply)

  private[wasm] val idchar = Syntax.charIn(
    ('0' to '9') ++
      ('a' to 'z') ++
      ('A' to 'Z') ++
      Seq('!', '#', '$', '%', '&', '`', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '\\', '^', '_', '|', '~',
        '\''): _*
  ) ?? "idchar"

  private[wasm] val id: TextSyntax[Id] =
    (Syntax.char('~') ~> idchar.+).string
      .transform(Id.fromString, _.toString)
      .mapError(_ => SyntaxError.UnexpectedByte) ?? "id"

  private[wasm] val sign     = Syntax.charIn('+', '-').mapError(_ => SyntaxError.InvalidSign) ?? "sign"
  private[wasm] val digit    = Syntax
    .charIn('0' to '9': _*)
    .transformEither(
      c => Right(c.toString.toLong),
      {
        case n if n >= 0 && n <= 9 => Right(n.toString.head)
        case n                     => Left(SyntaxError.InvalidDigit)
      }
    ) ?? "digit"
  private[wasm] val hexdigit =
    (digit |
      Syntax
        .charIn('a', 'A')
        .mapError(_ => SyntaxError.InvalidDigit)
        .transformTo(_ => 10L, { case 10 => 'A' }, SyntaxError.InvalidDigit) |
      Syntax
        .charIn('b', 'B')
        .mapError(_ => SyntaxError.InvalidDigit)
        .transformTo(_ => 11L, { case 11 => 'B' }, SyntaxError.InvalidDigit) |
      Syntax
        .charIn('c', 'C')
        .mapError(_ => SyntaxError.InvalidDigit)
        .transformTo(_ => 12L, { case 12 => 'C' }, SyntaxError.InvalidDigit) |
      Syntax
        .charIn('d', 'D')
        .mapError(_ => SyntaxError.InvalidDigit)
        .transformTo(_ => 13L, { case 13 => 'D' }, SyntaxError.InvalidDigit) |
      Syntax
        .charIn('e', 'E')
        .mapError(_ => SyntaxError.InvalidDigit)
        .transformTo(_ => 14L, { case 14 => 'E' }, SyntaxError.InvalidDigit) |
      Syntax
        .charIn('f', 'F')
        .mapError(_ => SyntaxError.InvalidDigit)
        .transformTo(_ => 15L, { case 15 => 'F' }, SyntaxError.InvalidDigit)) ?? "hexdigit"

  private def decToLong(digits: Chunk[Long]): Long =
    digits.foldLeft(0L)((acc, d) => acc * 10 + d)

  private def longToDec(value: Long): Chunk[Long] = {
    val builder = ChunkBuilder.make[Long]()
    var v       = BigInt(value)
    if (v.signum < 0) v += (BigInt(1L) << 64)
    while (v > 0) {
      builder += v.mod(10).toLong
      v /= 10
    }
    builder.result().reverse
  }

  private def hexToLong(digits: Chunk[Long]): Long =
    digits.foldLeft(0L)((acc, d) => acc * 16 + d)

  private def longToHex(value: Long): Chunk[Long] = {
    val builder = ChunkBuilder.make[Long]()
    var v       = BigInt(value)
    if (v.signum < 0) v += (BigInt(1L) << 64)
    while (v > 0) {
      builder += v.mod(16).toLong
      v /= 16
    }
    builder.result().reverse
  }

  private[wasm] val num    = (digit.+).transform(decToLong, longToDec) ?? "num"
  private[wasm] val hexnum = (hexdigit.+).transform(hexToLong, longToHex) ?? "hexnum"

  private[wasm] val u64: TextSyntax[Long] = ((keyword("0x") ~> hexnum) | num) ?? "u64"
  private[wasm] val s64: TextSyntax[Long] = (sign.? ~ u64).transform(
    {
      case (None, n)      => n
      case (Some('-'), n) => -n
      case (Some('+'), n) => n
    },
    n => if (n >= 0) (None, n) else (Some('-'), -n)
  ) ?? "s64"
  private[wasm] val i64                   = s64 | u64 ?? "i64"

  private def longToInt(value: Long): Either[SyntaxError, Int] =
    if (value >= Int.MinValue && value <= Int.MaxValue) Right(value.toInt)
    else Left(SyntaxError.Int32TooLarge)

  private[wasm] val u32: TextSyntax[Int] = u64.transformEither(longToInt, i => Right(i.toLong)) ?? "u32"
  private[wasm] val s32: TextSyntax[Int] = s64.transformEither(longToInt, i => Right(i.toLong)) ?? "s32"
  private[wasm] val i32                  = s32 | u32 ?? "i32"

  private def longToByte(value: Long): Either[SyntaxError, Byte] =
    if (value >= Byte.MinValue && value <= Byte.MaxValue) Right(value.toByte)
    else Left(SyntaxError.Int8TooLarge)

  private[wasm] val u8: TextSyntax[Byte] = u64.transformEither(longToByte, i => Right(i.toLong)) ?? "u8"
  private[wasm] val s8: TextSyntax[Byte] = s64.transformEither(longToByte, i => Right(i.toLong)) ?? "s8"
  private[wasm] val i8                   = s8 | u8 ?? "i8"

  private def longToShort(value: Long): Either[SyntaxError, Short] =
    if (value >= Short.MinValue && value <= Short.MaxValue) Right(value.toShort)
    else Left(SyntaxError.Int16TooLarge)

  private[wasm] val u16: TextSyntax[Short] = u64.transformEither(longToShort, i => Right(i.toLong)) ?? "u16"
  private[wasm] val s16: TextSyntax[Short] = s64.transformEither(longToShort, i => Right(i.toLong)) ?? "s16"
  private[wasm] val i16                    = s16 | u16 ?? "i16"

  private def decToFrac(digits: Chunk[Long]): Double =
    digits.foldRight(BigDecimal(0.0))((acc, d) => (d + acc) / 10.0).toDouble

  private def fracToDec(value: Double): Chunk[Long] =
    Chunk.fromIterable(value.toString.split('.').last.map(ch => ch.toString.toLong))

  private def hexToFrac(digits: Chunk[Long]): Double =
    digits.foldRight(BigDecimal(0.0))((acc, d) => (d + acc) / 16.0).toDouble // TODO

  private def fracToHex(value: Double): Chunk[Long] =
    longToHex(value.toString.split('.').last.toLong) // TODO

  private[wasm] val frac    = digit
    .repeatWithSep(Syntax.char('_').mapError(_ => SyntaxError.UnexpectedByte).?.unit(None))
    .transform(decToFrac, fracToDec) ?? "frac"
  private[wasm] val hexfrac =
    hexdigit
      .repeatWithSep(Syntax.char('_').mapError(_ => SyntaxError.UnexpectedByte).?.unit(None))
      .transform(hexToFrac, fracToHex) ?? "hexfrac"
  private[wasm] val float =
//    (num ~ Syntax.char('.') ~ frac ~ Syntax.charIn('e', 'E') ~ sign.? ~ num).transform(
//      { case (l, _, _, sign, e) => ??? },
//      { case d => ??? }
//    ) |
//      (num ~ Syntax.char('.').? ~ Syntax.charIn('e', 'E') ~ sign.? ~ num).transform(
//        { case (l, _, _, sign, e) => ??? },
//        { case d => ??? }
//      ) |
    (num ~ Syntax.char('.').mapError(_ => SyntaxError.UnexpectedByte) ~ frac)
      .transform({ case (l, d) => l.toDouble + d }, d => (d.toLong, d - d.toLong)) // |
//      (num ~ Syntax.char('.').?).transform(_.toDouble, _.toLong) ?? "float" // TODO

  private[wasm] val f64mag = (
    float |
      keyword("inf").as(Double.PositiveInfinity) |
      keyword("nan").as(Double.NaN)
  ) ?? "f64mag" // TODO: or hexFloat or nan(n)

  private[wasm] val f64 = (sign.? ~ f64mag).transform(
    {
      case (None, n)      => n
      case (Some('-'), n) => -n
      case (Some('+'), n) => n
    },
    (n: Double) => if (n >= 0) (None, n) else (Some('-'), -n)
  ) ?? "f64"

  private[wasm] val f32mag = (
    float.transform(_.toFloat, _.toDouble) |
      keyword("inf").as(Float.PositiveInfinity) |
      keyword("nan").as(Float.NaN)
  ) ?? "f64mag" // TODO: or hexFloat or nan(n)

  private[wasm] val f32 = (sign.? ~ f32mag).transform(
    {
      case (None, n)      => n
      case (Some('-'), n) => -n
      case (Some('+'), n) => n
    },
    (n: Float) => if (n >= 0) (None, n) else (Some('-'), -n)
  ) ?? "f32"

  private def int128FromI8x16(values: Chunk[Byte]): Either[SyntaxError, Int128]  =
    if (values.length == 16)
      Right(
        Int128.i8x16(
          values(0),
          values(1),
          values(2),
          values(3),
          values(4),
          values(5),
          values(6),
          values(7),
          values(8),
          values(9),
          values(10),
          values(11),
          values(12),
          values(13),
          values(14),
          values(15)
        )
      )
    else Left(SyntaxError.UnexpectedNumberOfIntegers)
  private def int128ToI8x16(value: Int128): Either[SyntaxError, Chunk[Byte]]     = {
    val (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16) = value.toI8x16
    Right(Chunk(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16))
  }
  private def int128FromI16x8(values: Chunk[Short]): Either[SyntaxError, Int128] =
    if (values.length == 8)
      Right(Int128.i16x8(values(0), values(1), values(2), values(3), values(4), values(5), values(6), values(7)))
    else Left(SyntaxError.UnexpectedNumberOfIntegers)
  private def int128ToI16x8(value: Int128): Either[SyntaxError, Chunk[Short]]    = {
    val (s1, s2, s3, s4, s5, s6, s7, s8) = value.toI16x8
    Right(Chunk(s1, s2, s3, s4, s5, s6, s7, s8))
  }
  private def int128FromI32x4(values: Chunk[Int]): Either[SyntaxError, Int128]   =
    if (values.length == 4) Right(Int128.i32x4(values(0), values(1), values(2), values(3)))
    else Left(SyntaxError.UnexpectedNumberOfIntegers)

  private def int128ToI32x4(value: Int128): Either[SyntaxError, Chunk[Int]]     = {
    val (i1, i2, i3, i4) = value.toI32x4
    Right(Chunk(i1, i2, i3, i4))
  }
  private def int128FromI64x2(values: Chunk[Long]): Either[SyntaxError, Int128] =
    if (values.length == 2) Right(Int128.i64x2(values(0), values(1)))
    else Left(SyntaxError.UnexpectedNumberOfIntegers)

  private def int128ToI64x2(value: Int128): Either[SyntaxError, Chunk[Long]]      = {
    val (l1, l2) = value.toI64x2
    Right(Chunk(l1, l2))
  }
  private def int128FromF32x4(values: Chunk[Float]): Either[SyntaxError, Int128]  =
    if (values.length == 4) Right(Int128.f32x4(values(0), values(1), values(2), values(3)))
    else Left(SyntaxError.UnexpectedNumberOfIntegers)
  private def int128ToF32x4(value: Int128): Either[SyntaxError, Chunk[Float]]     = {
    val (f1, f2, f3, f4) = value.toF32x4
    Right(Chunk(f1, f2, f3, f4))
  }
  private def int128FromF64x2(values: Chunk[Double]): Either[SyntaxError, Int128] =
    if (values.length == 2) Right(Int128.f64x2(values(0), values(1)))
    else Left(SyntaxError.UnexpectedNumberOfIntegers)
  private def int128ToF64x2(value: Int128): Either[SyntaxError, Chunk[Double]]    = {
    val (d1, d2) = value.toF64x2
    Right(Chunk(d1, d2))
  }

  private[wasm] val i128: TextSyntax[Int128] =
    (keyword("i8x16") ~ space ~ i8.repeatWithSep(space)).transformEither(int128FromI8x16, int128ToI8x16) |
      (keyword("i16x8") ~ space ~ i16.repeatWithSep(space)).transformEither(int128FromI16x8, int128ToI16x8) |
      (keyword("i32x4") ~ space ~ i32.repeatWithSep(space)).transformEither(int128FromI32x4, int128ToI32x4) |
      (keyword("i64x2") ~ space ~ i64.repeatWithSep(space)).transformEither(int128FromI64x2, int128ToI64x2) |
      (keyword("f32x4") ~ space ~ f32.repeatWithSep(space)).transformEither(int128FromF32x4, int128ToF32x4) |
      (keyword("f64x2") ~ space ~ f64.repeatWithSep(space)).transformEither(int128FromF64x2, int128ToF64x2) ?? "i128"

  private[wasm] val stringchar =
    (Syntax.charNotIn((0x00.toChar to 0x1f.toChar) ++ Seq(0x7f.toChar, '\\', '"'): _*) |
      Syntax.string("\\t", '\t') |
      Syntax.string("\\n", '\n') |
      Syntax.string("\\r", '\r') |
      Syntax.string("\\\"", '"') |
      Syntax.string("\\'", '\'') |
      (Syntax.string("\\u{", ()) ~> hexnum <~ Syntax.char('}')).transform(
        hex => hex.toChar,
        char => char.toInt
      )) ?? "stringchar"

  private[wasm] val stringelem =
    (stringchar |
      (Syntax.char('\\') ~> hexdigit ~ hexdigit).transform(
        { case (a, b) => (a * 16 + b).toChar },
        char => (char.toInt / 16, char.toInt % 16)
      )) ?? "stringelem"

  private[wasm] val string =
    stringelem.*.surroundedBy(Syntax.char('"')).transform(_.mkString, Chunk.fromIterable) ?? "string"

  private[wasm] val name = string

  private[wasm] val numtype: TextSyntax[NumType] = (
    Syntax.string("i32", NumType.I32) |
      Syntax.string("i64", NumType.I64) |
      Syntax.string("f32", NumType.F32) |
      Syntax.string("f64", NumType.F64)
  ).mapError(_ => SyntaxError.InvalidNumType) ?? "numtype"

  private[wasm] val vectype: TextSyntax[VecType] = (
    Syntax.string("v128", VecType.V128)
  ).mapError(_ => SyntaxError.InvalidVecType) ?? "vectype"

  private[wasm] val reftype: TextSyntax[RefType] = (
    Syntax.string("funcref", RefType.FuncRef) |
      Syntax.string("externref", RefType.ExternRef)
  ).mapError(_ => SyntaxError.InvalidRefType) ?? "reftype"

  private[wasm] val heaptype: TextSyntax[RefType] = (
    Syntax.string("func", RefType.FuncRef) |
      Syntax.string("extern", RefType.ExternRef)
  ).mapError(_ => SyntaxError.InvalidRefType) ?? "heaptype"

  private[wasm] val valtype: TextSyntax[ValType] =
    (numtype.orElseU(vectype).orElseU(reftype)) ?? "valtype"

  private[wasm] val param =
    ((space0 ~ keyword("param") ~> space ~ id ~ space ~ valtype <~ space0).between(lpar, rpar) <+>
      (space0 ~ keyword("param") ~> space ~> vec(valtype) <~ space0).between(lpar, rpar)) ?? "param"

  private[wasm] val result =
    (space0 ~ keyword("result") ~> space ~> vec(valtype) <~ space0).between(lpar, rpar) ?? "result"

  private def paramsToResultType(value: Chunk[Either[(Id, ValType), Chunk[ValType]]]): ResultType =
    ResultType(value.flatMap {
      case Left((_, vt)) => Chunk.single(vt)
      case Right(vts)    => vts
    })

  private def resultsToResultType(value: Chunk[Chunk[ValType]]): ResultType =
    ResultType(value.flatten)

  private[wasm] val functype: TextSyntax[FuncType] =
    (space0 ~ keyword("func") ~> space ~> vec(param) ~ space ~ vec(result) <~ space0)
      .between(lpar, rpar)
      .transform(
        { case (ps, rs) => FuncType(paramsToResultType(ps), resultsToResultType(rs)) },
        ft =>
          (
            if (ft.input.values.isEmpty) Chunk.empty else Chunk(Right(ft.input.values)),
            if (ft.output.values.isEmpty) Chunk.empty else Chunk(ft.output.values)
          )
      )

  private[wasm] val limits: TextSyntax[Limits] = (u32 ~ (space ~> u32).?).of[Limits] ?? "limits"

  private[wasm] val memtype: TextSyntax[MemType]       = limits.of[MemType] ?? "memtype"
  private[wasm] val tabletype: TextSyntax[TableType]   = (limits ~ space ~ reftype).of[TableType] ?? "tabletype"
  private[wasm] val globaltype: TextSyntax[GlobalType] =
    ((space0 ~ keyword("mut") ~> space ~> valtype <~ space0)
      .between(lpar, rpar)
      .transformTo(
        vt => GlobalType(Mut.Var, vt),
        { case GlobalType(Mut.Var, vt) => vt },
        SyntaxError.InvalidGlobalType
      ) |
      valtype.transformTo(
        GlobalType(Mut.Const, _),
        { case GlobalType(Mut.Const, vt) => vt },
        SyntaxError.InvalidGlobalType
      )) ?? "globaltype"

  private[wasm] val typeidx =
    (id.transformEither(
      (id: Id) => ctx.lookupType(id).toRight(SyntaxError.InvalidId),
      (typeIdx: TypeIdx) => ctx.lookupTypeIdx(typeIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(TypeIdx.fromInt, _.toInt)) ?? "typeidx"

  private[wasm] val funcidx =
    (id.transformEither(
      (id: Id) => ctx.lookupFunc(id).toRight(SyntaxError.InvalidId),
      (funcIdx: FuncIdx) => ctx.lookupFuncIdx(funcIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(FuncIdx.fromInt, _.toInt)) ?? "funcidx"

  private[wasm] val tableidx =
    (id.transformEither(
      (id: Id) => ctx.lookupTable(id).toRight(SyntaxError.InvalidId),
      (tableIdx: TableIdx) => ctx.lookupTableIdx(tableIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(TableIdx.fromInt, _.toInt)) ?? "tableidx"

  private[wasm] val memidx =
    (id.transformEither(
      (id: Id) => ctx.lookupMem(id).toRight(SyntaxError.InvalidId),
      (memIdx: MemIdx) => ctx.lookupMemIdx(memIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(MemIdx.fromInt, _.toInt)) ?? "memidx"

  private[wasm] val globalidx =
    (id.transformEither(
      (id: Id) => ctx.lookupGlobal(id).toRight(SyntaxError.InvalidId),
      (globalIdx: GlobalIdx) => ctx.lookupGlobalIdx(globalIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(GlobalIdx.fromInt, _.toInt)) ?? "globalidx"

  private[wasm] val localidx =
    (id.transformEither(
      (id: Id) => ctx.lookupLocal(id).toRight(SyntaxError.InvalidId),
      (localIdx: LocalIdx) => ctx.lookupLocalIdx(localIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(LocalIdx.fromInt, _.toInt)) ?? "localidx"

  private[wasm] val labelidx =
    (id.transformEither(
      (id: Id) => ctx.lookupLabel(id).toRight(SyntaxError.InvalidId),
      (labelIdx: LabelIdx) => ctx.lookupLabelIdx(labelIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(LabelIdx.fromInt, _.toInt)) ?? "labelidx"

  private[wasm] val elemidx =
    (id.transformEither(
      (id: Id) => ctx.lookupElem(id).toRight(SyntaxError.InvalidId),
      (elemIdx: ElemIdx) => ctx.lookupElemIdx(elemIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(ElemIdx.fromInt, _.toInt)) ?? "elemidx"

  private[wasm] val dataidx =
    (id.transformEither(
      (id: Id) => ctx.lookupData(id).toRight(SyntaxError.InvalidId),
      (dataIdx: DataIdx) => ctx.lookupDataIdx(dataIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(DataIdx.fromInt, _.toInt)) ?? "dataidx"

  private[wasm] val typeuse: TextSyntax[TypeIdx] =
    Syntax.fail(SyntaxError.UnexpectedByte).asInstanceOf[TextSyntax[TypeIdx]] // TODO

  private[wasm] val laneidx: TextSyntax[LaneIdx] = u8.transform(LaneIdx.fromByte, _.toByte) ?? "laneidx"

  private[wasm] val offset = keyword("offset=") ~> u32 ?? "offset"
  private[wasm] val align  = keyword("align=") ~> u32 ?? "align"

  private[wasm] val memarg: TextSyntax[MemArg] =
    (offset.? ~ (space ~ align).?).transformTo(
      (o, a) => MemArg(o.getOrElse(0), a.getOrElse(0)),
      { case MemArg(offset, align) => (Some(offset), Some(align)) },
      SyntaxError.InvalidMemArg
    ) ?? "memarg"

  private object Instructions {

    private def op0(name: String, instr: Instr): TextSyntax[Instr] =
      keyword(name).as(instr)

    private def op1[I <: Instr: ClassTag, A](
        name: String,
        p1: TextSyntax[A],
        create: A => I,
        extract: I => A
    ): TextSyntax[Instr] =
      (keyword(name) ~> space ~> p1).transformTo(
        create,
        { case i: I => extract(i) },
        SyntaxError.InvalidInstruction
      )

    private def op1Custom[I <: Instr, A](
        name: String,
        p1: TextSyntax[A],
        create: A => I,
        extract: PartialFunction[Instr, A]
    ): TextSyntax[Instr] =
      (keyword(name) ~> space ~> p1).transformTo(
        create,
        extract,
        SyntaxError.InvalidInstruction
      )

    private def op2[I <: Instr: ClassTag, A, B](
        name: String,
        p1: TextSyntax[A],
        p2: TextSyntax[B],
        create: (A, B) => I,
        extract: I => (A, B)
    ): TextSyntax[Instr] =
      (keyword(name) ~> space ~> p1 ~ space ~ p2).transformTo(
        create(_, _),
        { case i: I => extract(i) },
        SyntaxError.InvalidInstruction
      )

    private def op2Custom[I <: Instr: ClassTag, A, B](
        name: String,
        p1: TextSyntax[A],
        p2: TextSyntax[B],
        create: (A, B) => I,
        extract: PartialFunction[Instr, (A, B)]
    ): TextSyntax[Instr] =
      (keyword(name) ~> space ~> p1 ~ space ~ p2).transformTo(
        create(_, _),
        extract,
        SyntaxError.InvalidInstruction
      )

    val control =
      op0("unreachable", Instr.Unreachable) |
        op0("nop", Instr.Nop) |
        op1[Instr.Br, LabelIdx]("br", labelidx, Instr.Br.apply, _.labelIdx) |
        op1[Instr.BrIf, LabelIdx]("br_if", labelidx, Instr.BrIf.apply, _.labelIdx) |
        op2[Instr.BrTable, Chunk[LabelIdx], LabelIdx](
          "br_table",
          vec(labelidx),
          labelidx,
          Instr.BrTable.apply,
          i => (i.labels, i.default)
        ) |
        op0("return", Instr.Return) |
        op1[Instr.Call, FuncIdx]("call", funcidx, Instr.Call.apply, _.idx) |
        op2[Instr.CallIndirect, Option[TableIdx], TypeIdx](
          "call_indirect",
          tableidx.?,
          typeuse,
          (tableIdx, typeIdx) => Instr.CallIndirect(tableIdx.getOrElse(TableIdx.fromInt(0)), typeIdx),
          i => (Some(i.tableIdx), i.typeIdx)
        )

    val ref =
      op1[Instr.RefNull, RefType]("ref.null", heaptype, Instr.RefNull.apply, _.refType) |
        op0("ref.is_null", Instr.RefIsNull) |
        op1[Instr.RefFunc, FuncIdx]("ref.func", funcidx, Instr.RefFunc.apply, _.idx)

    val parametric =
      op0("drop", Instr.Drop) |
        op1[Instr.Select, Option[Chunk[ValType]]]("select", result.?, Instr.Select.apply, _.types)

    val table =
      op1[Instr.LocalGet, LocalIdx]("local.get", localidx, Instr.LocalGet.apply, _.idx) |
        op1[Instr.LocalSet, LocalIdx]("local.set", localidx, Instr.LocalSet.apply, _.idx) |
        op1[Instr.LocalTee, LocalIdx]("local.tee", localidx, Instr.LocalTee.apply, _.idx) |
        op1[Instr.GlobalGet, GlobalIdx]("global.get", globalidx, Instr.GlobalGet.apply, _.idx) |
        op1[Instr.GlobalSet, GlobalIdx]("global.set", globalidx, Instr.GlobalSet.apply, _.idx) |
        op1[Instr.TableGet, Option[TableIdx]](
          "table.get",
          tableidx.?,
          idx => Instr.TableGet(idx.getOrElse(TableIdx.fromInt(0))),
          i => Some(i.idx)
        ) |
        op1[Instr.TableSet, Option[TableIdx]](
          "table.set",
          tableidx.?,
          idx => Instr.TableSet(idx.getOrElse(TableIdx.fromInt(0))),
          i => Some(i.idx)
        ) |
        op1[Instr.TableSize, Option[TableIdx]](
          "table.size",
          tableidx.?,
          idx => Instr.TableSize(idx.getOrElse(TableIdx.fromInt(0))),
          i => Some(i.idx)
        ) |
        op1[Instr.TableGrow, Option[TableIdx]](
          "table.grow",
          tableidx.?,
          idx => Instr.TableGrow(idx.getOrElse(TableIdx.fromInt(0))),
          i => Some(i.idx)
        ) |
        op1[Instr.TableFill, Option[TableIdx]](
          "table.fill",
          tableidx.?,
          idx => Instr.TableFill(idx.getOrElse(TableIdx.fromInt(0))),
          i => Some(i.idx)
        ) |
        op2[Instr.TableCopy, Option[TableIdx], Option[TableIdx]](
          "table.copy",
          tableidx.?,
          tableidx.?,
          (s, d) => Instr.TableCopy(s.getOrElse(TableIdx.fromInt(0)), d.getOrElse(TableIdx.fromInt(0))),
          i => (Some(i.source), Some(i.destination))
        ) |
        op2[Instr.TableInit, Option[TableIdx], ElemIdx](
          "table.init",
          tableidx.?,
          elemidx,
          (tableIdx, elemIdx) => Instr.TableInit(tableIdx.getOrElse(TableIdx.fromInt(0)), elemIdx),
          i => (Some(i.tableIdx), i.elemIdx)
        ) |
        op1[Instr.ElemDrop, ElemIdx]("elem.drop", elemidx, Instr.ElemDrop.apply, _.idx)

    val memory =
      op1Custom[Instr.Load, MemArg](
        "i32.load",
        memarg,
        Instr.Load(NumType.I32, _),
        { case Instr.Load(NumType.I32, memarg) => memarg }
      ) |
        op1Custom[Instr.Load, MemArg](
          "i64.load",
          memarg,
          Instr.Load(NumType.I64, _),
          { case Instr.Load(NumType.I64, memarg) => memarg }
        ) |
        op1Custom[Instr.Load, MemArg](
          "f32.load",
          memarg,
          Instr.Load(NumType.F32, _),
          { case Instr.Load(NumType.F32, memarg) => memarg }
        ) |
        op1Custom[Instr.Load, MemArg](
          "f64.load",
          memarg,
          Instr.Load(NumType.F64, _),
          { case Instr.Load(NumType.F64, memarg) => memarg }
        ) |
        op1Custom[Instr.Load8, MemArg](
          "i32.load8_s",
          memarg,
          Instr.Load8(NumType.I32, Signedness.Signed, _),
          { case Instr.Load8(NumType.I32, Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.Load8, MemArg](
          "i32.load8_u",
          memarg,
          Instr.Load8(NumType.I32, Signedness.Unsigned, _),
          { case Instr.Load8(NumType.I32, Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.Load8, MemArg](
          "i64.load8_s",
          memarg,
          Instr.Load8(NumType.I64, Signedness.Signed, _),
          { case Instr.Load8(NumType.I64, Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.Load8, MemArg](
          "i64.load8_u",
          memarg,
          Instr.Load8(NumType.I64, Signedness.Unsigned, _),
          { case Instr.Load8(NumType.I64, Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.Load16, MemArg](
          "i32.load16_s",
          memarg,
          Instr.Load16(NumType.I32, Signedness.Signed, _),
          { case Instr.Load16(NumType.I32, Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.Load16, MemArg](
          "i32.load16_u",
          memarg,
          Instr.Load16(NumType.I32, Signedness.Unsigned, _),
          { case Instr.Load16(NumType.I32, Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.Load16, MemArg](
          "i64.load16_s",
          memarg,
          Instr.Load16(NumType.I64, Signedness.Signed, _),
          { case Instr.Load16(NumType.I64, Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.Load16, MemArg](
          "i64.load16_u",
          memarg,
          Instr.Load16(NumType.I64, Signedness.Unsigned, _),
          { case Instr.Load16(NumType.I64, Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.Load32, MemArg](
          "i64.load32_s",
          memarg,
          Instr.Load32(Signedness.Signed, _),
          { case Instr.Load32(Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.Load32, MemArg](
          "i64.load32_u",
          memarg,
          Instr.Load32(Signedness.Unsigned, _),
          { case Instr.Load32(Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.Store, MemArg](
          "i32.store",
          memarg,
          Instr.Store(NumType.I32, _),
          { case Instr.Store(NumType.I32, memarg) => memarg }
        ) |
        op1Custom[Instr.Store, MemArg](
          "i64.store",
          memarg,
          Instr.Store(NumType.I64, _),
          { case Instr.Store(NumType.I64, memarg) => memarg }
        ) |
        op1Custom[Instr.Store, MemArg](
          "f32.store",
          memarg,
          Instr.Store(NumType.F32, _),
          { case Instr.Store(NumType.F32, memarg) => memarg }
        ) |
        op1Custom[Instr.Store, MemArg](
          "f64.store",
          memarg,
          Instr.Store(NumType.F64, _),
          { case Instr.Store(NumType.F64, memarg) => memarg }
        ) |
        op1Custom[Instr.Store8, MemArg](
          "i32.store8",
          memarg,
          Instr.Store8(NumType.I32, _),
          { case Instr.Store8(NumType.I32, memarg) => memarg }
        ) |
        op1Custom[Instr.Store8, MemArg](
          "i64.store8",
          memarg,
          Instr.Store8(NumType.I64, _),
          { case Instr.Store8(NumType.I64, memarg) => memarg }
        ) |
        op1Custom[Instr.Store16, MemArg](
          "i32.store16",
          memarg,
          Instr.Store16(NumType.I32, _),
          { case Instr.Store16(NumType.I32, memarg) => memarg }
        ) |
        op1Custom[Instr.Store16, MemArg](
          "i64.store16",
          memarg,
          Instr.Store16(NumType.I64, _),
          { case Instr.Store16(NumType.I64, memarg) => memarg }
        ) |
        op1Custom[Instr.Store32, MemArg](
          "i64.store32",
          memarg,
          Instr.Store32(_),
          { case Instr.Store32(memarg) => memarg }
        ) |
        op0("memory.size", Instr.MemorySize) |
        op0("memory.grow", Instr.MemoryGrow) |
        op0("memory.fill", Instr.MemoryFill) |
        op0("memory.copy", Instr.MemoryCopy) |
        op1[Instr.MemoryInit, DataIdx](
          "memory.init",
          dataidx,
          Instr.MemoryInit(_),
          _.idx
        ) |
        op1[Instr.DataDrop, DataIdx](
          "data.drop",
          dataidx,
          Instr.DataDrop(_),
          _.idx
        )

    val numeric =
      op1[Instr.I32Const, Int]("i32.const", s32, Instr.I32Const(_), _.value) |
        op1[Instr.I64Const, Long]("i64.const", s64, Instr.I64Const(_), _.value) |
        op1[Instr.F32Const, Float]("f32.const", f32, Instr.F32Const(_), _.value) |
        op1[Instr.F64Const, Double]("f64.const", f64, Instr.F64Const(_), _.value) |
        op0("i32.clz", Instr.IClz(IntWidth.I32)) |
        op0("i32.ctz", Instr.ICtz(IntWidth.I32)) |
        op0("i32.popcnt", Instr.IPopCnt(IntWidth.I32)) |
        op0("i32.add", Instr.IAdd(IntWidth.I32)) |
        op0("i32.sub", Instr.ISub(IntWidth.I32)) |
        op0("i32.mul", Instr.IMul(IntWidth.I32)) |
        op0("i32.div_s", Instr.IDiv(IntWidth.I32, Signedness.Signed)) |
        op0("i32.div_u", Instr.IDiv(IntWidth.I32, Signedness.Unsigned)) |
        op0("i32.rem_s", Instr.IRem(IntWidth.I32, Signedness.Signed)) |
        op0("i32.rem_u", Instr.IRem(IntWidth.I32, Signedness.Unsigned)) |
        op0("i32.and", Instr.IAnd(IntWidth.I32)) |
        op0("i32.or", Instr.IOr(IntWidth.I32)) |
        op0("i32.xor", Instr.IXor(IntWidth.I32)) |
        op0("i32.shl", Instr.IShl(IntWidth.I32)) |
        op0("i32.shr_s", Instr.IShr(IntWidth.I32, Signedness.Signed)) |
        op0("i32.shr_u", Instr.IShr(IntWidth.I32, Signedness.Unsigned)) |
        op0("i32.rotl", Instr.IRotL(IntWidth.I32)) |
        op0("i32.rotr", Instr.IRotR(IntWidth.I32)) |
        op0("i64.clz", Instr.IClz(IntWidth.I64)) |
        op0("i64.ctz", Instr.ICtz(IntWidth.I64)) |
        op0("i64.popcnt", Instr.IPopCnt(IntWidth.I64)) |
        op0("i64.add", Instr.IAdd(IntWidth.I64)) |
        op0("i64.sub", Instr.ISub(IntWidth.I64)) |
        op0("i64.mul", Instr.IMul(IntWidth.I64)) |
        op0("i64.div_s", Instr.IDiv(IntWidth.I64, Signedness.Signed)) |
        op0("i64.div_u", Instr.IDiv(IntWidth.I64, Signedness.Unsigned)) |
        op0("i64.rem_s", Instr.IRem(IntWidth.I64, Signedness.Signed)) |
        op0("i64.rem_u", Instr.IRem(IntWidth.I64, Signedness.Unsigned)) |
        op0("i64.and", Instr.IAnd(IntWidth.I64)) |
        op0("i64.or", Instr.IOr(IntWidth.I64)) |
        op0("i64.xor", Instr.IXor(IntWidth.I64)) |
        op0("i64.shl", Instr.IShl(IntWidth.I64)) |
        op0("i64.shr_s", Instr.IShr(IntWidth.I64, Signedness.Signed)) |
        op0("i64.shr_u", Instr.IShr(IntWidth.I64, Signedness.Unsigned)) |
        op0("i64.rotl", Instr.IRotL(IntWidth.I64)) |
        op0("i64.rotr", Instr.IRotR(IntWidth.I64)) |
        op0("f32.abs", Instr.FAbs(FloatWidth.F32)) |
        op0("f32.neg", Instr.FNeg(FloatWidth.F32)) |
        op0("f32.ceil", Instr.FCeil(FloatWidth.F32)) |
        op0("f32.floor", Instr.FFloor(FloatWidth.F32)) |
        op0("f32.trunc", Instr.FTrunc(FloatWidth.F32)) |
        op0("f32.nearest", Instr.FNearest(FloatWidth.F32)) |
        op0("f32.sqrt", Instr.FSqrt(FloatWidth.F32)) |
        op0("f32.add", Instr.FAdd(FloatWidth.F32)) |
        op0("f32.sub", Instr.FSub(FloatWidth.F32)) |
        op0("f32.mul", Instr.FMul(FloatWidth.F32)) |
        op0("f32.div", Instr.FDiv(FloatWidth.F32)) |
        op0("f32.min", Instr.FMin(FloatWidth.F32)) |
        op0("f32.max", Instr.FMax(FloatWidth.F32)) |
        op0("f32.copysign", Instr.FCopySign(FloatWidth.F32)) |
        op0("f64.abs", Instr.FAbs(FloatWidth.F64)) |
        op0("f64.neg", Instr.FNeg(FloatWidth.F64)) |
        op0("f64.ceil", Instr.FCeil(FloatWidth.F64)) |
        op0("f64.floor", Instr.FFloor(FloatWidth.F64)) |
        op0("f64.trunc", Instr.FTrunc(FloatWidth.F64)) |
        op0("f64.nearest", Instr.FNearest(FloatWidth.F64)) |
        op0("f64.sqrt", Instr.FSqrt(FloatWidth.F64)) |
        op0("f64.add", Instr.FAdd(FloatWidth.F64)) |
        op0("f64.sub", Instr.FSub(FloatWidth.F64)) |
        op0("f64.mul", Instr.FMul(FloatWidth.F64)) |
        op0("f64.div", Instr.FDiv(FloatWidth.F64)) |
        op0("f64.min", Instr.FMin(FloatWidth.F64)) |
        op0("f64.max", Instr.FMax(FloatWidth.F64)) |
        op0("f64.copysign", Instr.FCopySign(FloatWidth.F64)) |
        op0("i32.eqz", Instr.IEqz(IntWidth.I32)) |
        op0("i32.eq", Instr.IEq(IntWidth.I32)) |
        op0("i32.ne", Instr.INe(IntWidth.I32)) |
        op0("i32.lt_s", Instr.ILt(IntWidth.I32, Signedness.Signed)) |
        op0("i32.lt_u", Instr.ILt(IntWidth.I32, Signedness.Unsigned)) |
        op0("i32.gt_s", Instr.IGt(IntWidth.I32, Signedness.Signed)) |
        op0("i32.gt_u", Instr.IGt(IntWidth.I32, Signedness.Unsigned)) |
        op0("i32.le_s", Instr.ILe(IntWidth.I32, Signedness.Signed)) |
        op0("i32.le_u", Instr.ILe(IntWidth.I32, Signedness.Unsigned)) |
        op0("i32.ge_s", Instr.IGe(IntWidth.I32, Signedness.Signed)) |
        op0("i32.ge_u", Instr.IGe(IntWidth.I32, Signedness.Unsigned)) |
        op0("i64.eq", Instr.IEq(IntWidth.I64)) |
        op0("i64.ne", Instr.INe(IntWidth.I64)) |
        op0("i64.lt_s", Instr.ILt(IntWidth.I64, Signedness.Signed)) |
        op0("i64.lt_u", Instr.ILt(IntWidth.I64, Signedness.Unsigned)) |
        op0("i64.gt_s", Instr.IGt(IntWidth.I64, Signedness.Signed)) |
        op0("i64.gt_u", Instr.IGt(IntWidth.I64, Signedness.Unsigned)) |
        op0("i64.le_s", Instr.ILe(IntWidth.I64, Signedness.Signed)) |
        op0("i64.le_u", Instr.ILe(IntWidth.I64, Signedness.Unsigned)) |
        op0("i64.ge_s", Instr.IGe(IntWidth.I64, Signedness.Signed)) |
        op0("i64.ge_u", Instr.IGe(IntWidth.I64, Signedness.Unsigned)) |
        op0("f32.eq", Instr.FEq(FloatWidth.F32)) |
        op0("f32.ne", Instr.FNe(FloatWidth.F32)) |
        op0("f32.lt", Instr.FLt(FloatWidth.F32)) |
        op0("f32.gt", Instr.FGt(FloatWidth.F32)) |
        op0("f32.le", Instr.FLe(FloatWidth.F32)) |
        op0("f32.ge", Instr.FGe(FloatWidth.F32)) |
        op0("f64.eq", Instr.FEq(FloatWidth.F64)) |
        op0("f64.ne", Instr.FNe(FloatWidth.F64)) |
        op0("f64.lt", Instr.FLt(FloatWidth.F64)) |
        op0("f64.gt", Instr.FGt(FloatWidth.F64)) |
        op0("f64.le", Instr.FLe(FloatWidth.F64)) |
        op0("f64.ge", Instr.FGe(FloatWidth.F64))

    val numericConversions =
      op0("i32.wrap_i64", Instr.I32WrapI64) |
        op0("i32.trunc_f32_s", Instr.ITruncF(IntWidth.I32, FloatWidth.F32, Signedness.Signed)) |
        op0("i32.trunc_f32_u", Instr.ITruncF(IntWidth.I32, FloatWidth.F32, Signedness.Unsigned)) |
        op0("i32.trunc_f64_s", Instr.ITruncF(IntWidth.I32, FloatWidth.F64, Signedness.Signed)) |
        op0("i32.trunc_f64_u", Instr.ITruncF(IntWidth.I32, FloatWidth.F64, Signedness.Unsigned)) |
        op0("i32.trunc_sat_f32_s", Instr.ITruncSatF(IntWidth.I32, FloatWidth.F32, Signedness.Signed)) |
        op0("i32.trunc_sat_f32_u", Instr.ITruncSatF(IntWidth.I32, FloatWidth.F32, Signedness.Unsigned)) |
        op0("i32.trunc_sat_f64_s", Instr.ITruncSatF(IntWidth.I32, FloatWidth.F64, Signedness.Signed)) |
        op0("i32.trunc_sat_f64_u", Instr.ITruncSatF(IntWidth.I32, FloatWidth.F64, Signedness.Unsigned)) |
        op0("i64.extend_i32_s", Instr.I64ExtendI32(Signedness.Signed)) |
        op0("i64.extend_i32_u", Instr.I64ExtendI32(Signedness.Unsigned)) |
        op0("i64.trunc_f32_s", Instr.ITruncF(IntWidth.I64, FloatWidth.F32, Signedness.Signed)) |
        op0("i64.trunc_f32_u", Instr.ITruncF(IntWidth.I64, FloatWidth.F32, Signedness.Unsigned)) |
        op0("i64.trunc_f64_s", Instr.ITruncF(IntWidth.I64, FloatWidth.F64, Signedness.Signed)) |
        op0("i64.trunc_f64_u", Instr.ITruncF(IntWidth.I64, FloatWidth.F64, Signedness.Unsigned)) |
        op0("i64.trunc_sat_f32_s", Instr.ITruncSatF(IntWidth.I64, FloatWidth.F32, Signedness.Signed)) |
        op0("i64.trunc_sat_f32_u", Instr.ITruncSatF(IntWidth.I64, FloatWidth.F32, Signedness.Unsigned)) |
        op0("i64.trunc_sat_f64_s", Instr.ITruncSatF(IntWidth.I64, FloatWidth.F64, Signedness.Signed)) |
        op0("i64.trunc_sat_f64_u", Instr.ITruncSatF(IntWidth.I64, FloatWidth.F64, Signedness.Unsigned)) |
        op0("f32.convert_i32_s", Instr.FConvertI(FloatWidth.F32, IntWidth.I32, Signedness.Signed)) |
        op0("f32.convert_i32_u", Instr.FConvertI(FloatWidth.F32, IntWidth.I32, Signedness.Unsigned)) |
        op0("f32.convert_i64_s", Instr.FConvertI(FloatWidth.F32, IntWidth.I64, Signedness.Signed)) |
        op0("f32.convert_i64_u", Instr.FConvertI(FloatWidth.F32, IntWidth.I64, Signedness.Unsigned)) |
        op0("f32.demote_f64", Instr.F32DemoteF64) |
        op0("f64.convert_i32_s", Instr.FConvertI(FloatWidth.F64, IntWidth.I32, Signedness.Signed)) |
        op0("f64.convert_i32_u", Instr.FConvertI(FloatWidth.F64, IntWidth.I32, Signedness.Unsigned)) |
        op0("f64.convert_i64_s", Instr.FConvertI(FloatWidth.F64, IntWidth.I64, Signedness.Signed)) |
        op0("f64.convert_i64_u", Instr.FConvertI(FloatWidth.F64, IntWidth.I64, Signedness.Unsigned)) |
        op0("f64.promote_f32", Instr.F64PromoteF32) |
        op0("i32.reinterpret_f32", Instr.IReinterpretF(IntWidth.I32)) |
        op0("i64.reinterpret_f64", Instr.IReinterpretF(IntWidth.I64)) |
        op0("f32.reinterpret_i32", Instr.FReinterpretI(FloatWidth.F32)) |
        op0("f64.reinterpret_i64", Instr.FReinterpretI(FloatWidth.F64)) |
        op0("i32.extend8_s", Instr.IExtend8S(IntWidth.I32)) |
        op0("i32.extend16_s", Instr.IExtend16S(IntWidth.I32)) |
        op0("i64.extend8_s", Instr.IExtend8S(IntWidth.I64)) |
        op0("i64.extend16_s", Instr.IExtend16S(IntWidth.I64)) |
        op0("i64.extend32_s", Instr.I64Extend32S)

    val vectorLoadStore =
      op1Custom[Instr.Load, MemArg](
        "v128.load",
        memarg,
        Instr.Load(VecType.V128, _),
        { case Instr.Load(VecType.V128, memarg) => memarg }
      ) |
        op1Custom[Instr.V128Load8x8, MemArg](
          "v128.load8x8_s",
          memarg,
          Instr.V128Load8x8(Signedness.Signed, _),
          { case Instr.V128Load8x8(Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.V128Load8x8, MemArg](
          "v128.load8x8_u",
          memarg,
          Instr.V128Load8x8(Signedness.Unsigned, _),
          { case Instr.V128Load8x8(Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.V128Load16x4, MemArg](
          "v128.load16x4_s",
          memarg,
          Instr.V128Load16x4(Signedness.Signed, _),
          { case Instr.V128Load16x4(Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.V128Load16x4, MemArg](
          "v128.load16x4_u",
          memarg,
          Instr.V128Load16x4(Signedness.Unsigned, _),
          { case Instr.V128Load16x4(Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.V128Load32x2, MemArg](
          "v128.load32x2_s",
          memarg,
          Instr.V128Load32x2(Signedness.Signed, _),
          { case Instr.V128Load32x2(Signedness.Signed, memarg) => memarg }
        ) |
        op1Custom[Instr.V128Load32x2, MemArg](
          "v128.load32x2_u",
          memarg,
          Instr.V128Load32x2(Signedness.Unsigned, _),
          { case Instr.V128Load32x2(Signedness.Unsigned, memarg) => memarg }
        ) |
        op1Custom[Instr.V128LoadSplat, MemArg](
          "v128.load8_splat",
          memarg,
          Instr.V128LoadSplat(VectorLoadShape.WW8, _),
          { case Instr.V128LoadSplat(VectorLoadShape.WW8, memarg) => memarg }
        ) |
        op1Custom[Instr.V128LoadSplat, MemArg](
          "v128.load16_splat",
          memarg,
          Instr.V128LoadSplat(VectorLoadShape.WW16, _),
          { case Instr.V128LoadSplat(VectorLoadShape.WW16, memarg) => memarg }
        ) |
        op1Custom[Instr.V128LoadSplat, MemArg](
          "v128.load32_splat",
          memarg,
          Instr.V128LoadSplat(VectorLoadShape.WW32, _),
          { case Instr.V128LoadSplat(VectorLoadShape.WW32, memarg) => memarg }
        ) |
        op1Custom[Instr.V128LoadSplat, MemArg](
          "v128.load64_splat",
          memarg,
          Instr.V128LoadSplat(VectorLoadShape.WW64, _),
          { case Instr.V128LoadSplat(VectorLoadShape.WW64, memarg) => memarg }
        ) |
        op1[Instr.V128Load32Zero, MemArg]("v128.load32_zero", memarg, Instr.V128Load32Zero.apply, _.memArg) |
        op1[Instr.V128Load64Zero, MemArg]("v128.load64_zero", memarg, Instr.V128Load64Zero.apply, _.memArg) |
        op1Custom[Instr.Store, MemArg](
          "v128.store",
          memarg,
          Instr.Store(VecType.V128, _),
          { case Instr.Store(VecType.V128, memarg) => memarg }
        ) |
        op2Custom[Instr.V128LoadLane, MemArg, LaneIdx](
          "v128.load8_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128LoadLane(VectorLoadShape.WW8, memarg, laneidx),
          { case Instr.V128LoadLane(VectorLoadShape.WW8, memarg, laneidx) => (memarg, laneidx) }
        ) |
        op2Custom[Instr.V128LoadLane, MemArg, LaneIdx](
          "v128.load16_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128LoadLane(VectorLoadShape.WW16, memarg, laneidx),
          { case Instr.V128LoadLane(VectorLoadShape.WW16, memarg, laneidx) => (memarg, laneidx) }
        ) |
        op2Custom[Instr.V128LoadLane, MemArg, LaneIdx](
          "v128.load32_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128LoadLane(VectorLoadShape.WW32, memarg, laneidx),
          { case Instr.V128LoadLane(VectorLoadShape.WW32, memarg, laneidx) => (memarg, laneidx) }
        ) |
        op2Custom[Instr.V128LoadLane, MemArg, LaneIdx](
          "v128.load64_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128LoadLane(VectorLoadShape.WW64, memarg, laneidx),
          { case Instr.V128LoadLane(VectorLoadShape.WW64, memarg, laneidx) => (memarg, laneidx) }
        ) |
        op2Custom[Instr.V128StoreLane, MemArg, LaneIdx](
          "v128.store8_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128StoreLane(VectorLoadShape.WW8, memarg, laneidx),
          { case Instr.V128StoreLane(VectorLoadShape.WW8, memarg, laneidx) => (memarg, laneidx) }
        ) |
        op2Custom[Instr.V128StoreLane, MemArg, LaneIdx](
          "v128.store16_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128StoreLane(VectorLoadShape.WW16, memarg, laneidx),
          { case Instr.V128StoreLane(VectorLoadShape.WW16, memarg, laneidx) => (memarg, laneidx) }
        ) |
        op2Custom[Instr.V128StoreLane, MemArg, LaneIdx](
          "v128.store32_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128StoreLane(VectorLoadShape.WW32, memarg, laneidx),
          { case Instr.V128StoreLane(VectorLoadShape.WW32, memarg, laneidx) => (memarg, laneidx) }
        ) |
        op2Custom[Instr.V128StoreLane, MemArg, LaneIdx](
          "v128.store64_lane",
          memarg,
          laneidx,
          (memarg, laneidx) => Instr.V128StoreLane(VectorLoadShape.WW64, memarg, laneidx),
          { case Instr.V128StoreLane(VectorLoadShape.WW64, memarg, laneidx) => (memarg, laneidx) }
        )

    val vector1 =
      op1[Instr.V128Const, Int128]("v128.const", i128, Instr.V128Const.apply, _.value) |
        (keyword(
          "i8x16.shuffle"
        ) ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx ~ space ~ laneidx)
          .transformTo(
            Instr.VI8x16Shuffle.apply,
            { case i: Instr.VI8x16Shuffle =>
              (
                i.laneIdx0,
                i.laneIdx1,
                i.laneIdx2,
                i.laneIdx3,
                i.laneIdx4,
                i.laneIdx5,
                i.laneIdx6,
                i.laneIdx7,
                i.laneIdx8,
                i.laneIdx9,
                i.laneIdx10,
                i.laneIdx11,
                i.laneIdx12,
                i.laneIdx13,
                i.laneIdx14,
                i.laneIdx15
              )
            },
            SyntaxError.InvalidInstruction
          ) |
        op0("i8x16.swizzle", Instr.VI18x16Swizzle) |
        op0("i8x16.splat", Instr.VSplat(IShape.I8x16)) |
        op0("i16x8.splat", Instr.VSplat(IShape.I16x8)) |
        op0("i32x4.splat", Instr.VSplat(IShape.I32x4)) |
        op0("i64x2.splat", Instr.VSplat(IShape.I64x2)) |
        op0("f32x4.splat", Instr.VSplat(FShape.F32x4)) |
        op0("f64x2.splat", Instr.VSplat(FShape.F64x2))

    val vector2 =
      op1Custom[Instr.VI8x16ExtractLane, LaneIdx](
        "i8x16.extract_lane_s",
        laneidx,
        Instr.VI8x16ExtractLane(Signedness.Signed, _),
        { case Instr.VI8x16ExtractLane(Signedness.Signed, laneidx) => laneidx }
      ) |
        op1Custom[Instr.VI8x16ExtractLane, LaneIdx](
          "i8x16.extract_lane_u",
          laneidx,
          Instr.VI8x16ExtractLane(Signedness.Unsigned, _),
          { case Instr.VI8x16ExtractLane(Signedness.Unsigned, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VReplaceLane, LaneIdx](
          "i8x16.replace_lane",
          laneidx,
          Instr.VReplaceLane(IShape.I8x16, _),
          { case Instr.VReplaceLane(IShape.I8x16, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VI16x8ExtractLane, LaneIdx](
          "i16x8.extract_lane_s",
          laneidx,
          Instr.VI16x8ExtractLane(Signedness.Signed, _),
          { case Instr.VI16x8ExtractLane(Signedness.Signed, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VI16x8ExtractLane, LaneIdx](
          "i16x8.extract_lane_u",
          laneidx,
          Instr.VI16x8ExtractLane(Signedness.Unsigned, _),
          { case Instr.VI16x8ExtractLane(Signedness.Unsigned, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VReplaceLane, LaneIdx](
          "i16x8.replace_lane",
          laneidx,
          Instr.VReplaceLane(IShape.I16x8, _),
          { case Instr.VReplaceLane(IShape.I16x8, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VI32x4ExtractLane, LaneIdx](
          "i32x4.extract_lane",
          laneidx,
          Instr.VI32x4ExtractLane(_),
          { case Instr.VI32x4ExtractLane(laneidx) => laneidx }
        ) |
        op1Custom[Instr.VReplaceLane, LaneIdx](
          "i32x4.replace_lane",
          laneidx,
          Instr.VReplaceLane(IShape.I32x4, _),
          { case Instr.VReplaceLane(IShape.I32x4, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VI64x2ExtractLane, LaneIdx](
          "i64x2.extract_lane",
          laneidx,
          Instr.VI64x2ExtractLane(_),
          { case Instr.VI64x2ExtractLane(laneidx) => laneidx }
        ) |
        op1Custom[Instr.VReplaceLane, LaneIdx](
          "i64x2.replace_lane",
          laneidx,
          Instr.VReplaceLane(IShape.I64x2, _),
          { case Instr.VReplaceLane(IShape.I64x2, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VFExtractLane, LaneIdx](
          "f32x4.extract_lane",
          laneidx,
          Instr.VFExtractLane(FShape.F32x4, _),
          { case Instr.VFExtractLane(FShape.F32x4, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VReplaceLane, LaneIdx](
          "f32x4.replace_lane",
          laneidx,
          Instr.VReplaceLane(FShape.F32x4, _),
          { case Instr.VReplaceLane(FShape.F32x4, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VFExtractLane, LaneIdx](
          "f64x2.extract_lane",
          laneidx,
          Instr.VFExtractLane(FShape.F64x2, _),
          { case Instr.VFExtractLane(FShape.F64x2, laneidx) => laneidx }
        ) |
        op1Custom[Instr.VReplaceLane, LaneIdx](
          "f64x2.replace_lane",
          laneidx,
          Instr.VReplaceLane(FShape.F64x2, _),
          { case Instr.VReplaceLane(FShape.F64x2, laneidx) => laneidx }
        )

    val vectorCompareI8x16 =
      op0("i8x16.eq", Instr.VIEq(IShape.I8x16)) |
        op0("i8x16.ne", Instr.VINe(IShape.I8x16)) |
        op0("i8x16.lt_s", Instr.VILt(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.lt_u", Instr.VILt(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.le_s", Instr.VILe(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.le_u", Instr.VILe(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.gt_s", Instr.VIGt(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.gt_u", Instr.VIGt(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.ge_s", Instr.VIGe(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.ge_u", Instr.VIGe(IShape.I8x16, Signedness.Unsigned))

    val vectorCompareI16x8 =
      op0("i16x8.eq", Instr.VIEq(IShape.I16x8)) |
        op0("i16x8.ne", Instr.VINe(IShape.I16x8)) |
        op0("i16x8.lt_s", Instr.VILt(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.lt_u", Instr.VILt(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.le_s", Instr.VILe(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.le_u", Instr.VILe(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.gt_s", Instr.VIGt(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.gt_u", Instr.VIGt(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.ge_s", Instr.VIGe(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.ge_u", Instr.VIGe(IShape.I16x8, Signedness.Unsigned))

    val vectorCompareI32x4 =
      op0("i32x4.eq", Instr.VIEq(IShape.I32x4)) |
        op0("i32x4.ne", Instr.VINe(IShape.I32x4)) |
        op0("i32x4.lt_s", Instr.VILt(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.lt_u", Instr.VILt(IShape.I32x4, Signedness.Unsigned)) |
        op0("i32x4.le_s", Instr.VILe(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.le_u", Instr.VILe(IShape.I32x4, Signedness.Unsigned)) |
        op0("i32x4.gt_s", Instr.VIGt(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.gt_u", Instr.VIGt(IShape.I32x4, Signedness.Unsigned)) |
        op0("i32x4.ge_s", Instr.VIGe(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.ge_u", Instr.VIGe(IShape.I32x4, Signedness.Unsigned))

    val vectorCompareI64x2 =
      op0("i64x2.eq", Instr.VIEq(IShape.I64x2)) |
        op0("i64x2.ne", Instr.VINe(IShape.I64x2)) |
        op0("i64x2.lt", Instr.VI64x2Lt) |
        op0("i64x2.le", Instr.VI64x2Le) |
        op0("i64x2.gt", Instr.VI64x2Gt) |
        op0("i64x2.ge", Instr.VI64x2Ge)

    val vectorCompareF32x4 =
      op0("f32x4.eq", Instr.VFEq(FShape.F32x4)) |
        op0("f32x4.ne", Instr.VFNe(FShape.F32x4)) |
        op0("f32x4.lt", Instr.VFLt(FShape.F32x4)) |
        op0("f32x4.le", Instr.VFLe(FShape.F32x4)) |
        op0("f32x4.gt", Instr.VFGt(FShape.F32x4)) |
        op0("f32x4.ge", Instr.VFGe(FShape.F32x4))

    val vectorCompareF64x2 =
      op0("f64x2.eq", Instr.VFEq(FShape.F64x2)) |
        op0("f64x2.ne", Instr.VFNe(FShape.F64x2)) |
        op0("f64x2.lt", Instr.VFLt(FShape.F64x2)) |
        op0("f64x2.le", Instr.VFLe(FShape.F64x2)) |
        op0("f64x2.gt", Instr.VFGt(FShape.F64x2)) |
        op0("f64x2.ge", Instr.VFGe(FShape.F64x2))

    val vectorBinary =
      op0("v128.not", Instr.V128Not) |
        op0("v128.and", Instr.V128And) |
        op0("v128.andnot", Instr.V128AndNot) |
        op0("v128.or", Instr.V128Or) |
        op0("v128.xor", Instr.V128XOr) |
        op0("v128.bitselect", Instr.V128BitSelect) |
        op0("v128.any_true", Instr.V128AnyTrue)

    val vectorI8x16 =
      op0("i8x16.abs", Instr.VIAbs(IShape.I8x16)) |
        op0("i8x16.neg", Instr.VINeg(IShape.I8x16)) |
        op0("i8x16.all_true", Instr.VIAllTrue(IShape.I8x16)) |
        op0("i8x16.bitmask", Instr.VIBitMask(IShape.I8x16)) |
        op0("i8x16.narrow_i16x8_s", Instr.VI8x16NarrowI16x8(Signedness.Signed)) |
        op0("i8x16.narrow_i16x8_u", Instr.VI8x16NarrowI16x8(Signedness.Unsigned)) |
        op0("i8x16.shl", Instr.VIShl(IShape.I8x16)) |
        op0("i8x16.shr_s", Instr.VIShr(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.shr_u", Instr.VIShr(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.add", Instr.VIAdd(IShape.I8x16)) |
        op0("i8x16.add_sat_s", Instr.VIAddSat(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.add_sat_u", Instr.VIAddSat(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.sub", Instr.VISub(IShape.I8x16)) |
        op0("i8x16.sub_sat_s", Instr.VISubSat(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.sub_sat_u", Instr.VISubSat(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.min_s", Instr.VIMin(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.min_u", Instr.VIMin(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.max_s", Instr.VIMax(IShape.I8x16, Signedness.Signed)) |
        op0("i8x16.max_u", Instr.VIMax(IShape.I8x16, Signedness.Unsigned)) |
        op0("i8x16.avgr_u", Instr.VIAvgr(IShape.I8x16)) |
        op0("i8x16.popcnt", Instr.VI8x16PopCnt)

    val vectorI16x8 =
      op0("i16x8.abs", Instr.VIAbs(IShape.I16x8)) |
        op0("i16x8.neg", Instr.VINeg(IShape.I16x8)) |
        op0("i16x8.all_true", Instr.VIAllTrue(IShape.I16x8)) |
        op0("i16x8.bitmask", Instr.VIBitMask(IShape.I16x8)) |
        op0("i16x8.narrow_i32x4_s", Instr.VI16x8NarrowI32x4(Signedness.Signed)) |
        op0("i16x8.narrow_i32x4_u", Instr.VI16x8NarrowI32x4(Signedness.Unsigned)) |
        op0("i16x8.shl", Instr.VIShl(IShape.I16x8)) |
        op0("i16x8.shr_s", Instr.VIShr(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.shr_u", Instr.VIShr(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.add", Instr.VIAdd(IShape.I16x8)) |
        op0("i16x8.add_sat_s", Instr.VIAddSat(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.add_sat_u", Instr.VIAddSat(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.sub", Instr.VISub(IShape.I16x8)) |
        op0("i16x8.sub_sat_s", Instr.VISubSat(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.sub_sat_u", Instr.VISubSat(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.mul", Instr.VIMul(IShape.I16x8)) |
        op0("i16x8.min_s", Instr.VIMin(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.min_u", Instr.VIMin(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.max_s", Instr.VIMax(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.max_u", Instr.VIMax(IShape.I16x8, Signedness.Unsigned)) |
        op0("i16x8.avgr_u", Instr.VIAvgr(IShape.I16x8)) |
        op0("i16x8.q15mulr_sat_s", Instr.VI16x8Q15MulrSat) |
        op0("i16x8.extmul_low_i8x16_s", Instr.VIExtMul(IShape.I16x8, Half.Low, Signedness.Signed)) |
        op0("i16x8.extmul_low_i8x16_u", Instr.VIExtMul(IShape.I16x8, Half.Low, Signedness.Unsigned)) |
        op0("i16x8.extmul_high_i8x16_s", Instr.VIExtMul(IShape.I16x8, Half.High, Signedness.Signed)) |
        op0("i16x8.extmul_high_i8x16_u", Instr.VIExtMul(IShape.I16x8, Half.High, Signedness.Unsigned)) |
        op0("i16x8.extadd_pairwise_i8x16_s", Instr.VIExtAddPairwise(IShape.I16x8, Signedness.Signed)) |
        op0("i16x8.extadd_pairwise_i8x16_u", Instr.VIExtAddPairwise(IShape.I16x8, Signedness.Unsigned))

    val vectorI32x4 =
      op0("i32x4.abs", Instr.VIAbs(IShape.I32x4)) |
        op0("i32x4.neg", Instr.VINeg(IShape.I32x4)) |
        op0("i32x4.all_true", Instr.VIAllTrue(IShape.I32x4)) |
        op0("i32x4.bitmask", Instr.VIBitMask(IShape.I32x4)) |
        op0("i32x4.extadd_pairwise_i16x8_s", Instr.VIExtAddPairwise(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.extend_low_i16x8_s", Instr.VI32x4ExtendI16x8(Half.Low, Signedness.Signed)) |
        op0("i32x4.extend_high_i16x8_s", Instr.VI32x4ExtendI16x8(Half.High, Signedness.Signed)) |
        op0("i32x4.extend_low_i16x8_u", Instr.VI32x4ExtendI16x8(Half.Low, Signedness.Unsigned)) |
        op0("i32x4.extend_high_i16x8_u", Instr.VI32x4ExtendI16x8(Half.High, Signedness.Unsigned)) |
        op0("i32x4.shl", Instr.VIShl(IShape.I32x4)) |
        op0("i32x4.shr_s", Instr.VIShr(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.shr_u", Instr.VIShr(IShape.I32x4, Signedness.Unsigned)) |
        op0("i32x4.add", Instr.VIAdd(IShape.I32x4)) |
        op0("i32x4.sub", Instr.VISub(IShape.I32x4)) |
        op0("i32x4.mul", Instr.VIMul(IShape.I32x4)) |
        op0("i32x4.min_s", Instr.VIMin(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.min_u", Instr.VIMin(IShape.I32x4, Signedness.Unsigned)) |
        op0("i32x4.max_s", Instr.VIMax(IShape.I32x4, Signedness.Signed)) |
        op0("i32x4.max_u", Instr.VIMax(IShape.I32x4, Signedness.Unsigned)) |
        op0("i32x4.dot_i16x8_s", Instr.VI32x4DotI16x8) |
        op0("i32x4.extmul_low_i16x8_s", Instr.VIExtMul(IShape.I32x4, Half.Low, Signedness.Signed)) |
        op0("i32x4.extmul_low_i16x8_u", Instr.VIExtMul(IShape.I32x4, Half.Low, Signedness.Unsigned)) |
        op0("i32x4.extmul_high_i16x8_s", Instr.VIExtMul(IShape.I32x4, Half.High, Signedness.Signed)) |
        op0("i32x4.extmul_high_i16x8_u", Instr.VIExtMul(IShape.I32x4, Half.High, Signedness.Unsigned))

    val vectorI64x2 =
      op0("i64x2.abs", Instr.VIAbs(IShape.I64x2)) |
        op0("i64x2.neg", Instr.VINeg(IShape.I64x2)) |
        op0("i64x2.all_true", Instr.VIAllTrue(IShape.I64x2)) |
        op0("i64x2.bitmask", Instr.VIBitMask(IShape.I64x2)) |
        op0("i64x2.extend_low_i32x4_s", Instr.VI64x2ExtendI32x4(Half.Low, Signedness.Signed)) |
        op0("i64x2.extend_high_i32x4_s", Instr.VI64x2ExtendI32x4(Half.High, Signedness.Signed)) |
        op0("i64x2.extend_low_i32x4_u", Instr.VI64x2ExtendI32x4(Half.Low, Signedness.Unsigned)) |
        op0("i64x2.extend_high_i32x4_u", Instr.VI64x2ExtendI32x4(Half.High, Signedness.Unsigned)) |
        op0("i64x2.shl", Instr.VIShl(IShape.I64x2)) |
        op0("i64x2.shr_s", Instr.VIShr(IShape.I64x2, Signedness.Signed)) |
        op0("i64x2.shr_u", Instr.VIShr(IShape.I64x2, Signedness.Unsigned)) |
        op0("i64x2.add", Instr.VIAdd(IShape.I64x2)) |
        op0("i64x2.sub", Instr.VISub(IShape.I64x2)) |
        op0("i64x2.mul", Instr.VIMul(IShape.I64x2)) |
        op0("i64x2.extmul_low_i32x4_s", Instr.VIExtMul(IShape.I64x2, Half.Low, Signedness.Signed)) |
        op0("i64x2.extmul_low_i32x4_u", Instr.VIExtMul(IShape.I64x2, Half.Low, Signedness.Unsigned)) |
        op0("i64x2.extmul_high_i32x4_s", Instr.VIExtMul(IShape.I64x2, Half.High, Signedness.Signed)) |
        op0("i64x2.extmul_high_i32x4_u", Instr.VIExtMul(IShape.I64x2, Half.High, Signedness.Unsigned))

    val vectorF32x4 =
      op0("f32x4.abs", Instr.VFAbs(FShape.F32x4)) |
        op0("f32x4.neg", Instr.VFNeg(FShape.F32x4)) |
        op0("f32x4.sqrt", Instr.VFSqrt(FShape.F32x4)) |
        op0("f32x4.add", Instr.VFAdd(FShape.F32x4)) |
        op0("f32x4.sub", Instr.VFSub(FShape.F32x4)) |
        op0("f32x4.mul", Instr.VFMul(FShape.F32x4)) |
        op0("f32x4.div", Instr.VFDiv(FShape.F32x4)) |
        op0("f32x4.min", Instr.VFMin(FShape.F32x4)) |
        op0("f32x4.max", Instr.VFMax(FShape.F32x4)) |
        op0("f32x4.pmin", Instr.VFPMin(FShape.F32x4)) |
        op0("f32x4.pmax", Instr.VFPMax(FShape.F32x4))

    val vectorF64x2 =
      op0("f64x2.abs", Instr.VFAbs(FShape.F64x2)) |
        op0("f64x2.neg", Instr.VFNeg(FShape.F64x2)) |
        op0("f64x2.sqrt", Instr.VFSqrt(FShape.F64x2)) |
        op0("f64x2.add", Instr.VFAdd(FShape.F64x2)) |
        op0("f64x2.sub", Instr.VFSub(FShape.F64x2)) |
        op0("f64x2.mul", Instr.VFMul(FShape.F64x2)) |
        op0("f64x2.div", Instr.VFDiv(FShape.F64x2)) |
        op0("f64x2.min", Instr.VFMin(FShape.F64x2)) |
        op0("f64x2.max", Instr.VFMax(FShape.F64x2)) |
        op0("f64x2.pmin", Instr.VFPMin(FShape.F64x2)) |
        op0("f64x2.pmax", Instr.VFPMax(FShape.F64x2))

    val vectorConversions =
      op0("i32x4.trunc_sat_f32x4_s", Instr.VI32x4TruncSatF32x4(Signedness.Signed)) |
        op0("i32x4.trunc_sat_f32x4_u", Instr.VI32x4TruncSatF32x4(Signedness.Unsigned)) |
        op0("i32x4.trunc_sat_f64x2_s_zero", Instr.VI32x4TruncSatF64x2Zero(Signedness.Signed)) |
        op0("i32x4.trunc_sat_f64x2_u_zero", Instr.VI32x4TruncSatF64x2Zero(Signedness.Unsigned)) |
        op0("f32x4.convert_i32x4_s", Instr.VF32x4ConvertI32x4(Signedness.Signed)) |
        op0("f32x4.convert_i32x4_u", Instr.VF32x4ConvertI32x4(Signedness.Unsigned)) |
        op0("f64x2.convert_low_i32x4_s", Instr.VF64x2ConvertLowI32x4(Signedness.Signed)) |
        op0("f64x2.convert_low_i32x4_u", Instr.VF64x2ConvertLowI32x4(Signedness.Unsigned)) |
        op0("f32x4.demote_f64x2_zero", Instr.VF32x4DemoteF64x2Zero) |
        op0("f64x2.promote_low_f32x4", Instr.VF64x2PromoteLowI32x4)
  }

  private[wasm] val plaininstr: TextSyntax[Instr] =
    Instructions.control |
      Instructions.ref |
      Instructions.parametric |
      Instructions.table |
      Instructions.memory |
      Instructions.numeric |
      Instructions.numericConversions |
      Instructions.vectorLoadStore |
      Instructions.vector1 |
      Instructions.vector2 |
      Instructions.vectorCompareI8x16 |
      Instructions.vectorCompareI16x8 |
      Instructions.vectorCompareI32x4 |
      Instructions.vectorCompareI64x2 |
      Instructions.vectorCompareF32x4 |
      Instructions.vectorCompareF64x2 |
      Instructions.vectorBinary |
      Instructions.vectorI8x16 |
      Instructions.vectorI16x8 |
      Instructions.vectorI32x4 |
      Instructions.vectorI64x2 |
      Instructions.vectorF32x4 |
      Instructions.vectorF64x2 |
      Instructions.vectorConversions ?? "plaininstr"

  private[wasm] lazy val foldedinstr: TextSyntax[NonEmptyChunk[Instr]] =
    (lpar ~> plaininstr ~ space ~ foldedinstr.repeatWithSep0(space) <~ rpar)
      .transform(
        (instr, instrs) => NonEmptyChunk.fromChunk(instrs.flatten :+ instr).get,
        (instrs: NonEmptyChunk[Instr]) => (instrs.last, instrs.init.map(NonEmptyChunk.single))
      )

  private[wasm] lazy val instr: TextSyntax[Instr] =
    plaininstr ?? "instr" // TODO: blockinstr

  private[wasm] lazy val expr: TextSyntax[Chunk[Instr]] =
    (instr <+> foldedinstr)
      .repeatWithSep0(space)
      .transform(
        parts => parts.map(_.left.map(NonEmptyChunk.single).merge).flatten,
        instrs => instrs.map(i => Left(i))
      ) ?? "expr"
}

object Text {
  def apply(): Text = {
    given ctx: IdentifierContext = IdentifierContext.empty
    new Text
  }

  val defaultExtension: String = ".wat"

  type TextSyntax[A]  = Syntax[SyntaxError, Char, Char, A]
  type TextParser[A]  = Parser[SyntaxError, Char, A]
  type TextPrinter[A] = Printer[SyntaxError, Char, A]

  type Id = Id.Id

  object Id {
    opaque type Id = String

    def fromString(s: String): Id = s
  }

  final case class IdentifierContext(
      types: Chunk[Option[Id]],
      funcs: Chunk[Option[Id]],
      tables: Chunk[Option[Id]],
      mems: Chunk[Option[Id]],
      globals: Chunk[Option[Id]],
      elem: Chunk[Option[Id]],
      data: Chunk[Option[Id]],
      locals: Chunk[Option[Id]],
      labels: Chunk[Option[Id]],
      typedefs: Chunk[FuncType]
  ) {
    def lookupType(id: Id): Option[TypeIdx] =
      types.zipWithIndex.collectFirst { case (Some(`id`), i) => TypeIdx.fromInt(i) }

    def lookupTypeIdx(typeIdx: TypeIdx): Option[Id] =
      types.lift(typeIdx.toInt).flatten

    def lookupFunc(id: Id): Option[FuncIdx] =
      funcs.zipWithIndex.collectFirst { case (Some(`id`), i) => FuncIdx.fromInt(i) }

    def lookupFuncIdx(funcIdx: FuncIdx): Option[Id] =
      funcs.lift(funcIdx.toInt).flatten

    def lookupTable(id: Id): Option[TableIdx] =
      tables.zipWithIndex.collectFirst { case (Some(`id`), i) => TableIdx.fromInt(i) }

    def lookupTableIdx(tableIdx: TableIdx): Option[Id] =
      tables.lift(tableIdx.toInt).flatten

    def lookupMem(id: Id): Option[MemIdx] =
      mems.zipWithIndex.collectFirst { case (Some(`id`), i) => MemIdx.fromInt(i) }

    def lookupMemIdx(memIdx: MemIdx): Option[Id] =
      mems.lift(memIdx.toInt).flatten

    def lookupGlobal(id: Id): Option[GlobalIdx] =
      globals.zipWithIndex.collectFirst { case (Some(`id`), i) => GlobalIdx.fromInt(i) }

    def lookupGlobalIdx(globalIdx: GlobalIdx): Option[Id] =
      globals.lift(globalIdx.toInt).flatten

    def lookupElem(id: Id): Option[ElemIdx] =
      elem.zipWithIndex.collectFirst { case (Some(`id`), i) => ElemIdx.fromInt(i) }

    def lookupElemIdx(elemIdx: ElemIdx): Option[Id] =
      elem.lift(elemIdx.toInt).flatten

    def lookupData(id: Id): Option[DataIdx] =
      data.zipWithIndex.collectFirst { case (Some(`id`), i) => DataIdx.fromInt(i) }

    def lookupDataIdx(dataIdx: DataIdx): Option[Id] =
      data.lift(dataIdx.toInt).flatten

    def lookupLocal(id: Id): Option[LocalIdx] =
      locals.zipWithIndex.collectFirst { case (Some(`id`), i) => LocalIdx.fromInt(i) }

    def lookupLocalIdx(localIdx: LocalIdx): Option[Id] =
      locals.lift(localIdx.toInt).flatten

    def lookupLabel(id: Id): Option[LabelIdx] =
      labels.zipWithIndex.collectFirst { case (Some(`id`), i) => LabelIdx.fromInt(i) }

    def lookupLabelIdx(labelIdx: LabelIdx): Option[Id] =
      labels.lift(labelIdx.toInt).flatten
  }

  object IdentifierContext {
    val empty: IdentifierContext = IdentifierContext(
      Chunk.empty,
      Chunk.empty,
      Chunk.empty,
      Chunk.empty,
      Chunk.empty,
      Chunk.empty,
      Chunk.empty,
      Chunk.empty,
      Chunk.empty,
      Chunk.empty
    )
  }
}
