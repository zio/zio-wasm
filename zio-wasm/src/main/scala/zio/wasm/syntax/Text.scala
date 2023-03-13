package zio.wasm.syntax

import zio.{Chunk, ChunkBuilder}
import zio.parser.*
import zio.wasm.*

import scala.reflect.ClassTag

object Text {
  // TODO: clean up errors

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
      comment).*.unit(Chunk.single(' ')) ?? "space"

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

  private def longToInt(value: Long): Either[SyntaxError, Int] =
    if (value >= Int.MinValue && value <= Int.MaxValue) Right(value.toInt)
    else Left(SyntaxError.Int32TooLarge)

  private[wasm] val u32: TextSyntax[Int] =
    ((keyword("0x") ~> hexnum) | num).transformEither(longToInt, i => Right(i.toLong)) ?? "u32"
  private[wasm] val s32: TextSyntax[Int] = (sign.? ~ u32).transform(
    {
      case (None, n)      => n
      case (Some('-'), n) => -n
      case (Some('+'), n) => n
    },
    n => if (n >= 0) (None, n) else (Some('-'), -n)
  ) ?? "s32"

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

  private[wasm] def typeidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupType(id).toRight(SyntaxError.InvalidId),
      (typeIdx: TypeIdx) => ctx.lookupTypeIdx(typeIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(TypeIdx.fromInt, _.toInt)) ?? "typeidx"

  private[wasm] def funcidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupFunc(id).toRight(SyntaxError.InvalidId),
      (funcIdx: FuncIdx) => ctx.lookupFuncIdx(funcIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(FuncIdx.fromInt, _.toInt)) ?? "funcidx"

  private[wasm] def tableidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupTable(id).toRight(SyntaxError.InvalidId),
      (tableIdx: TableIdx) => ctx.lookupTableIdx(tableIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(TableIdx.fromInt, _.toInt)) ?? "tableidx"

  private[wasm] def memidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupMem(id).toRight(SyntaxError.InvalidId),
      (memIdx: MemIdx) => ctx.lookupMemIdx(memIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(MemIdx.fromInt, _.toInt)) ?? "memidx"

  private[wasm] def globalidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupGlobal(id).toRight(SyntaxError.InvalidId),
      (globalIdx: GlobalIdx) => ctx.lookupGlobalIdx(globalIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(GlobalIdx.fromInt, _.toInt)) ?? "globalidx"

  private[wasm] def localidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupLocal(id).toRight(SyntaxError.InvalidId),
      (localIdx: LocalIdx) => ctx.lookupLocalIdx(localIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(LocalIdx.fromInt, _.toInt)) ?? "localidx"

  private[wasm] def labelidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupLabel(id).toRight(SyntaxError.InvalidId),
      (labelIdx: LabelIdx) => ctx.lookupLabelIdx(labelIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(LabelIdx.fromInt, _.toInt)) ?? "labelidx"

  private[wasm] def elemidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupElem(id).toRight(SyntaxError.InvalidId),
      (elemIdx: ElemIdx) => ctx.lookupElemIdx(elemIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(ElemIdx.fromInt, _.toInt)) ?? "elemidx"

  private[wasm] def dataidx(using ctx: IdentifierContext) =
    (id.transformEither(
      (id: Id) => ctx.lookupData(id).toRight(SyntaxError.InvalidId),
      (dataIdx: DataIdx) => ctx.lookupDataIdx(dataIdx).toRight(SyntaxError.InvalidId)
    ) |
      u32.transform(DataIdx.fromInt, _.toInt)) ?? "dataidx"

  private[wasm] def typeuse(using ctx: IdentifierContext): TextSyntax[TypeIdx] =
    ??? // TODO

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

    def control(using IdentifierContext) =
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

    def ref(using IdentifierContext) =
      op1[Instr.RefNull, RefType]("ref.null", heaptype, Instr.RefNull.apply, _.refType) |
        op0("ref.is_null", Instr.RefIsNull) |
        op1[Instr.RefFunc, FuncIdx]("ref.func", funcidx, Instr.RefFunc.apply, _.idx)

    val parametric =
      op0("drop", Instr.Drop)
      op1[Instr.Select, Option[Chunk[ValType]]]("select", result.?, Instr.Select.apply, _.types)

    def table(using IdentifierContext) =
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

    def memory(using IdentifierContext) =
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
  }

  private[wasm] def plaininstr(using IdentifierContext): TextSyntax[Instr] =
    Instructions.control |
      Instructions.ref |
      Instructions.parametric |
      Instructions.table |
      Instructions.memory |
      Instructions.numeric
    // | TODO
}
