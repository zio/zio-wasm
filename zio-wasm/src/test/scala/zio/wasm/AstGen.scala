package zio.wasm

import zio.test.*

object AstGen {

  val int128: Gen[Any, Int128] =
    for {
      high <- Gen.long
      low  <- Gen.long
    } yield Int128(high, low)

  val numType: Gen[Any, NumType] = Gen.oneOf(
    Gen.const(NumType.I32),
    Gen.const(NumType.I64),
    Gen.const(NumType.F32),
    Gen.const(NumType.F64)
  )

  val vecType: Gen[Any, VecType] = Gen.oneOf(
    Gen.const(VecType.V128)
  )

  val refType: Gen[Any, RefType] = Gen.oneOf(
    Gen.const(RefType.FuncRef),
    Gen.const(RefType.ExternRef)
  )

  val valType: Gen[Any, ValType] =
    Gen.oneOf(
      numType,
      vecType,
      refType
    )

  val resultType: Gen[Any, ResultType] = Gen.chunkOf(valType).map(ResultType.apply)

  val funcType: Gen[Any, FuncType] =
    for {
      input  <- resultType
      output <- resultType
    } yield FuncType(input, output)

  val limits: Gen[Any, Limits] =
    for {
      min <- Gen.int
      max <- Gen.option(Gen.int)
    } yield Limits(min, max)

  val tableType: Gen[Any, TableType] =
    for {
      elemType <- refType
      limits   <- limits
    } yield TableType(limits, elemType)

  val mut: Gen[Any, Mut] = Gen.oneOf(
    Gen.const(Mut.Const),
    Gen.const(Mut.Var)
  )

  val globalType: Gen[Any, GlobalType] =
    for {
      valueType <- valType
      mut       <- mut
    } yield GlobalType(mut, valueType)

  val blockType: Gen[Any, BlockType] = Gen.oneOf(
    Gen.const(BlockType.None),
    valType.map(BlockType.Value.apply),
    Gen.int.map(i => BlockType.Index(TypeIdx.fromInt(i)))
  )

  val memArg: Gen[Any, MemArg] =
    for {
      align  <- Gen.int
      offset <- Gen.int
    } yield MemArg(align, offset)
}
