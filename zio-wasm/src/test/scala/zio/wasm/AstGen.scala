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

  val numOrVecType: Gen[Any, NumType | VecType] =
    Gen.oneOf(
      Gen.const(NumType.I32),
      Gen.const(NumType.I64),
      Gen.const(NumType.F32),
      Gen.const(NumType.F64),
      Gen.const(VecType.V128)
    )

  val num32or64: Gen[Any, NumType.I32.type | NumType.I64.type ] =
    Gen.oneOf(
      Gen.const(NumType.I32),
      Gen.const(NumType.I64)
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

  val laneIdx: Gen[Any, LaneIdx] =
    Gen.byte.map(LaneIdx.fromByte)

  val funcIdx: Gen[Any, FuncIdx] =
    Gen.int.map(FuncIdx.fromInt)

  val localIdx: Gen[Any, LocalIdx] =
    Gen.int.map(LocalIdx.fromInt)

  val globalIdx: Gen[Any, GlobalIdx] =
    Gen.int.map(GlobalIdx.fromInt)

  val tableIdx: Gen[Any, TableIdx] =
    Gen.int.map(TableIdx.fromInt)

  val elemIdx: Gen[Any, ElemIdx] =
    Gen.int.map(ElemIdx.fromInt)

  val dataIdx: Gen[Any, DataIdx] =
    Gen.int.map(DataIdx.fromInt)

  val labelIdx: Gen[Any, LabelIdx] =
    Gen.int.map(LabelIdx.fromInt)

  val typeIdx: Gen[Any, TypeIdx] =
    Gen.int.map(TypeIdx.fromInt)

  val memArg: Gen[Any, MemArg] =
    for {
      align  <- Gen.int
      offset <- Gen.int
    } yield MemArg(align, offset)

  val intWidth: Gen[Any, IntWidth] =
    Gen.oneOf(
      Gen.const(IntWidth.I32),
      Gen.const(IntWidth.I64)
    )

  val floatWidth: Gen[Any, FloatWidth] =
    Gen.oneOf(
      Gen.const(FloatWidth.F32),
      Gen.const(FloatWidth.F64)
    )

  val signedness: Gen[Any, Signedness] =
    Gen.oneOf(
      Gen.const(Signedness.Signed),
      Gen.const(Signedness.Unsigned)
    )

  val half: Gen[Any, Half] =
    Gen.oneOf(
      Gen.const(Half.Low),
      Gen.const(Half.High),
    )

  val ishape: Gen[Any, IShape] = Gen.oneOf(
    Gen.const(IShape.I8x16),
    Gen.const(IShape.I16x8),
    Gen.const(IShape.I32x4),
    Gen.const(IShape.I64x2),
  )

  val fshape: Gen[Any, FShape] = Gen.oneOf(
    Gen.const(FShape.F32x4),
    Gen.const(FShape.F64x2),
  )

  val shape: Gen[Any, Shape] = Gen.oneOf(
    Gen.const(IShape.I8x16),
    Gen.const(IShape.I16x8),
    Gen.const(IShape.I32x4),
    Gen.const(IShape.I64x2),
    Gen.const(FShape.F32x4),
    Gen.const(FShape.F64x2),
  )

  val ishape2: Gen[Any, IShape.I8x16.type | IShape.I16x8.type] =
    Gen.oneOf(Gen.const(IShape.I8x16), Gen.const(IShape.I16x8))

  val ishape3: Gen[Any, IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type] =
    Gen.oneOf(Gen.const(IShape.I8x16), Gen.const(IShape.I16x8), Gen.const(IShape.I32x4))

  val ishape4: Gen[Any, IShape.I16x8.type | IShape.I32x4.type | IShape.I64x2.type] =
    Gen.oneOf(Gen.const(IShape.I16x8), Gen.const(IShape.I32x4), Gen.const(IShape.I64x2))

  val ishape5: Gen[Any, IShape.I16x8.type | IShape.I32x4.type] =
    Gen.oneOf(Gen.const(IShape.I16x8), Gen.const(IShape.I32x4))

  val vectorLoadShape: Gen[Any, VectorLoadShape] =
    Gen.oneOf(
      Gen.const(VectorLoadShape.WW8),
      Gen.const(VectorLoadShape.WW16),
      Gen.const(VectorLoadShape.WW32),
      Gen.const(VectorLoadShape.WW64),
    )

  lazy val instr: Gen[Any, Instr] =
    Gen.oneOf(
      Gen.int.map(Instr.I32Const.apply),
      Gen.long.map(Instr.I64Const.apply),
      Gen.float.map(Instr.F32Const.apply),
      Gen.double.map(Instr.F64Const.apply),
      intWidth.map(Instr.IEqz.apply),
      intWidth.map(Instr.IEq.apply),
      intWidth.map(Instr.INe.apply),
      intWidth.zip(signedness).map(Instr.ILt.apply),
      intWidth.zip(signedness).map(Instr.IGt.apply),
      intWidth.zip(signedness).map(Instr.ILe.apply),
      intWidth.zip(signedness).map(Instr.IGe.apply),
      floatWidth.map(Instr.FEq.apply),
      floatWidth.map(Instr.FNe.apply),
      floatWidth.map(Instr.FLt.apply),
      floatWidth.map(Instr.FGt.apply),
      floatWidth.map(Instr.FLe.apply),
      floatWidth.map(Instr.FGe.apply),
      intWidth.map(Instr.IClz.apply),
      intWidth.map(Instr.ICtz.apply),
      intWidth.map(Instr.IPopCnt.apply),
      intWidth.map(Instr.IAdd.apply),
      intWidth.map(Instr.ISub.apply),
      intWidth.map(Instr.IMul.apply),
      intWidth.zip(signedness).map(Instr.IDiv.apply),
      intWidth.zip(signedness).map(Instr.IRem.apply),
      intWidth.map(Instr.IAnd.apply),
      intWidth.map(Instr.IOr.apply),
      intWidth.map(Instr.IXor.apply),
      intWidth.map(Instr.IShl.apply),
      intWidth.zip(signedness).map(Instr.IShr.apply),
      intWidth.map(Instr.IRotL.apply),
      intWidth.map(Instr.IRotR.apply),
      floatWidth.map(Instr.FAbs.apply),
      floatWidth.map(Instr.FNeg.apply),
      floatWidth.map(Instr.FCeil.apply),
      floatWidth.map(Instr.FFloor.apply),
      floatWidth.map(Instr.FTrunc.apply),
      floatWidth.map(Instr.FNearest.apply),
      floatWidth.map(Instr.FSqrt.apply),
      floatWidth.map(Instr.FAdd.apply),
      floatWidth.map(Instr.FSub.apply),
      floatWidth.map(Instr.FMul.apply),
      floatWidth.map(Instr.FDiv.apply),
      floatWidth.map(Instr.FMin.apply),
      floatWidth.map(Instr.FMax.apply),
      floatWidth.map(Instr.FCopySign.apply),
      Gen.const(Instr.I32WrapI64),
      intWidth.zip(floatWidth.zip(signedness)).map { case (iw, (fw, s)) => Instr.ITruncF(iw, fw, s) },
      signedness.map(Instr.I64ExtendI32.apply),
      Gen.const(Instr.I64Extend32S),
      intWidth.map(Instr.IExtend8S.apply),
      intWidth.map(Instr.IExtend16S.apply),
      floatWidth.zip(intWidth.zip(signedness)).map { case (fw, (iw, s)) => Instr.FConvertI(fw, iw, s) },
      Gen.const(Instr.F32DemoteF64),
      Gen.const(Instr.F64PromoteF32),
      intWidth.map(Instr.IReinterpretF.apply),
      floatWidth.map(Instr.FReinterpretI.apply),
      intWidth.zip(floatWidth.zip(signedness)).map { case (iw, (fw, s)) => Instr.ITruncSatF(iw, fw, s) },
      int128.map(Instr.V128Const.apply),
      Gen.const(Instr.V128Not),
      Gen.const(Instr.V128And),
      Gen.const(Instr.V128AndNot),
      Gen.const(Instr.V128Or),
      Gen.const(Instr.V128XOr),
      Gen.const(Instr.V128BitSelect),
      Gen.const(Instr.V128AnyTrue),
      for {
        laneIdx0 <- laneIdx
        laneIdx1 <- laneIdx
        laneIdx2 <- laneIdx
        laneIdx3 <- laneIdx
        laneIdx4 <- laneIdx
        laneIdx5 <- laneIdx
        laneIdx6 <- laneIdx
        laneIdx7 <- laneIdx
        laneIdx8 <- laneIdx
        laneIdx9 <- laneIdx
        laneIdx10 <- laneIdx
        laneIdx11 <- laneIdx
        laneIdx12 <- laneIdx
        laneIdx13 <- laneIdx
        laneIdx14 <- laneIdx
        laneIdx15 <- laneIdx
      } yield Instr.VI8x16Shuffle(
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
      ),
      Gen.const(Instr.VI18x16Swizzle),
      shape.map(Instr.VSplat.apply),
      signedness.zip(laneIdx).map(Instr.VI8x16ExtractLane.apply),
      signedness.zip(laneIdx).map(Instr.VI16x8ExtractLane.apply),
      laneIdx.map(Instr.VI32x4ExtractLane.apply),
      laneIdx.map(Instr.VI64x2ExtractLane.apply),
      fshape.zip(laneIdx).map(Instr.VFExtractLane.apply),
      shape.zip(laneIdx).map(Instr.VReplaceLane.apply),
      ishape.map(Instr.VIEq.apply),
      ishape.map(Instr.VINe.apply),
      ishape3.zip(signedness).map { case (is, s) => Instr.VILt(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type], s) },
      ishape3.zip(signedness).map { case (is, s) => Instr.VIGt(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type], s) },
      ishape3.zip(signedness).map { case (is, s) => Instr.VILe(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type], s) },
      ishape3.zip(signedness).map { case (is, s) => Instr.VIGe(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type], s) },
      Gen.const(Instr.VI64x2Lt),
      Gen.const(Instr.VI64x2Gt),
      Gen.const(Instr.VI64x2Le),
      Gen.const(Instr.VI64x2Ge),
      fshape.map(Instr.VFEq.apply),
        fshape.map(Instr.VFNe.apply),
        fshape.map(Instr.VFLt.apply),
        fshape.map(Instr.VFGt.apply),
        fshape.map(Instr.VFLe.apply),
        fshape.map(Instr.VFGe.apply),
      ishape.map(Instr.VIAbs.apply),
      ishape.map(Instr.VINeg.apply),
      Gen.const(Instr.VI8x16PopCnt),
      Gen.const(Instr.VI16x8Q15MulrSat),
      Gen.const(Instr.VI32x4DotI16x8),
      fshape.map(Instr.VFAbs.apply),
        fshape.map(Instr.VFNeg.apply),
        fshape.map(Instr.VFSqrt.apply),
        fshape.map(Instr.VFCeil.apply),
        fshape.map(Instr.VFFloor.apply),
        fshape.map(Instr.VFTrunc.apply),
        fshape.map(Instr.VFNearest.apply),
      ishape.map(Instr.VIAllTrue.apply),
      ishape.map(Instr.VIBitMask.apply),
      signedness.map(Instr.VI8x16NarrowI16x8.apply),
        signedness.map(Instr.VI16x8NarrowI32x4.apply),
      half.zip(signedness).map(Instr.VI16x8ExtendI8x16.apply),
        half.zip(signedness).map(Instr.VI32x4ExtendI16x8.apply),
      half.zip(signedness).map(Instr.VI64x2ExtendI32x4.apply),
      ishape.map(Instr.VIAdd.apply),
      ishape.map(Instr.VISub.apply),
      ishape3.zip(signedness).map { case (is, s) => Instr.VIMin(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type], s) },
      ishape3.zip(signedness).map { case (is, s) => Instr.VIMax(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type], s) },
      ishape2.zip(signedness).map { case (is, s) => Instr.VIAddSat(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type], s) },
      ishape2.zip(signedness).map { case (is, s) => Instr.VISubSat(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type], s) },
      ishape4.map(is => Instr.VIMul(is.asInstanceOf[IShape.I16x8.type | IShape.I32x4.type | IShape.I64x2.type])),
      ishape2.map(is => Instr.VIAvgr(is.asInstanceOf[IShape.I8x16.type | IShape.I16x8.type])),
      ishape4.zip(half.zip(signedness)).map { case (is, (h, s)) => Instr.VIExtMul(is.asInstanceOf[IShape.I16x8.type | IShape.I32x4.type | IShape.I64x2.type], h, s) },
      ishape5.zip(signedness).map { case (is, s) => Instr.VIExtAddPairwise(is.asInstanceOf[IShape.I16x8.type | IShape.I32x4.type], s) },
      fshape.map(Instr.VFAdd.apply),
      fshape.map(Instr.VFSub.apply),
      fshape.map(Instr.VFMul.apply),
      fshape.map(Instr.VFDiv.apply),
      fshape.map(Instr.VFMin.apply),
      fshape.map(Instr.VFMax.apply),
      fshape.map(Instr.VFPMin.apply),
      fshape.map(Instr.VFPMax.apply),
      signedness.map(Instr.VI32x4TruncSatF32x4.apply),
      signedness.map(Instr.VI32x4TruncSatF64x2Zero.apply),
      signedness.map(Instr.VI32x4ConvertI32x4.apply),
      Gen.const(Instr.VF32x4DemoteF64x2Zero),
      signedness.map(Instr.VF64x2ConvertLowI32x4.apply),
      Gen.const(Instr.VF64x2PromoteLowI32x4),
      refType.map(Instr.RefNull.apply),
      Gen.const(Instr.RefIsNull),
      funcIdx.map(Instr.RefFunc.apply),
      Gen.const(Instr.Drop),
      Gen.option(Gen.chunkOf(valType)).map(Instr.Select.apply),
      localIdx.map(Instr.LocalGet.apply),
      localIdx.map(Instr.LocalSet.apply),
      localIdx.map(Instr.LocalTee.apply),
      globalIdx.map(Instr.GlobalGet.apply),
      globalIdx.map(Instr.GlobalSet.apply),
      tableIdx.map(Instr.TableGet.apply),
      tableIdx.map(Instr.TableSet.apply),
      tableIdx.map(Instr.TableSize.apply),
      tableIdx.map(Instr.TableGrow.apply),
      tableIdx.map(Instr.TableFill.apply),
      tableIdx.zip(tableIdx).map(Instr.TableCopy.apply),
      tableIdx.zip(elemIdx).map(Instr.TableInit.apply),
      elemIdx.map(Instr.ElemDrop.apply),
      numOrVecType.zip(memArg).map(Instr.Load.apply),
      numOrVecType.zip(memArg).map(Instr.Store.apply),
      num32or64.zip(signedness).zip(memArg).map { case (n, s, m) => Instr.Load8(n.asInstanceOf[NumType.I32.type  | NumType.I64.type ], s, m) },
      num32or64.zip(signedness).zip(memArg).map { case (n, s, m) => Instr.Load16(n.asInstanceOf[NumType.I32.type  | NumType.I64.type ], s, m) },
      signedness.zip(memArg).map(Instr.Load32.apply),
      num32or64.zip(memArg).map { case (n, s) => Instr.Store8(n.asInstanceOf[NumType.I32.type  | NumType.I64.type ], s) },
      num32or64.zip(memArg).map { case (n, s) => Instr.Store16(n.asInstanceOf[NumType.I32.type  | NumType.I64.type ], s) },
      memArg.map(Instr.Store32.apply),
      signedness.zip(memArg).map(Instr.V128Load8x8.apply),
        signedness.zip(memArg).map(Instr.V128Load16x4.apply),
        signedness.zip(memArg).map(Instr.V128Load32x2.apply),
        memArg.map(Instr.V128Load32Zero.apply),
        memArg.map(Instr.V128Load64Zero.apply),
      vectorLoadShape.zip(memArg).map(Instr.V128LoadSplat.apply),
      vectorLoadShape.zip(memArg).zip(laneIdx).map(Instr.V128LoadLane.apply),
      vectorLoadShape.zip(memArg).zip(laneIdx).map(Instr.V128StoreLane.apply),
      Gen.const(Instr.MemorySize),
        Gen.const(Instr.MemoryGrow),
      Gen.const(Instr.MemoryFill),
        Gen.const(Instr.MemoryCopy),
      dataIdx.map(Instr.MemoryInit.apply),
        dataIdx.map(Instr.DataDrop.apply),
      Gen.const(Instr.Nop),
      Gen.const(Instr.Unreachable),
      Gen.suspend(blockType.zip(Gen.chunkOf(instr)).map(Instr.Block.apply)),
      Gen.suspend(blockType.zip(Gen.chunkOf(instr)).map(Instr.Loop.apply)),
      Gen.suspend(blockType.zip(Gen.chunkOf(instr)).zip(Gen.chunkOf(instr)).map(Instr.If.apply)),
      labelIdx.map(Instr.Br.apply),
      labelIdx.map(Instr.BrIf.apply),
      Gen.chunkOf(labelIdx).zip(labelIdx).map(Instr.BrTable.apply),
      Gen.const(Instr.Return),
      funcIdx.map(Instr.Call.apply),
      tableIdx.zip(typeIdx).map(Instr.CallIndirect.apply)
    )
}
