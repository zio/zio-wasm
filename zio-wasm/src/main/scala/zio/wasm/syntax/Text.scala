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

  private[wasm] val frac    = digit.repeatWithSep(Syntax.char('_').?.unit(None)).transform(decToFrac, fracToDec) ?? "frac"
  private[wasm] val hexfrac =
    hexdigit.repeatWithSep(Syntax.char('_').?.unit(None)).transform(hexToFrac, fracToHex) ?? "hexfrac"
  private[wasm] val float =
//    (num ~ Syntax.char('.') ~ frac ~ Syntax.charIn('e', 'E') ~ sign.? ~ num).transform(
//      { case (l, _, _, sign, e) => ??? },
//      { case d => ??? }
//    ) |
//      (num ~ Syntax.char('.').? ~ Syntax.charIn('e', 'E') ~ sign.? ~ num).transform(
//        { case (l, _, _, sign, e) => ??? },
//        { case d => ??? }
//      ) |
    (num ~ Syntax.char('.') ~ frac).transform({ case (l, d) => l.toDouble + d }, d => (d.toLong, d - d.toLong)) // |
//      (num ~ Syntax.char('.').?).transform(_.toDouble, _.toLong) ?? "float" // TODO

  private[wasm] val fNmag = (
    float |
      Syntax.string("inf", Double.PositiveInfinity) |
      Syntax.string("nan", Double.NaN)
  ) ?? "fNmag" // TODO: or hexFloat or nan(n)

  private[wasm] val fN = (sign.? ~ fNmag).transform(
    {
      case (None, n)      => n
      case (Some('-'), n) => -n
      case (Some('+'), n) => n
    },
    (n: Double) => if (n >= 0) (None, n) else (Some('-'), -n)
  ) ?? "fN"

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

  private[wasm] def typeuse(using ctx: IdentifierContext): TextSyntax[TypeIdx] =
    ??? // TODO

  private[wasm] def plaininstr(using ctx: IdentifierContext): TextSyntax[Instr] =
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
      ) |
      op1[Instr.RefNull, RefType]("ref.null", heaptype, Instr.RefNull.apply, _.refType) |
      op0("ref.is_null", Instr.RefIsNull) |
      op1[Instr.RefFunc, FuncIdx]("ref.func", funcidx, Instr.RefFunc.apply, _.idx) |
      op0("drop", Instr.Drop) |
      op1[Instr.Select, Option[Chunk[ValType]]]("select", result.?, Instr.Select.apply, _.types) |
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
    // | TODO
}
