package zio.wasm.componentmodel

import zio.test.Gen
import zio.wasm.AstGen as WasmAstGen

object AstGen {

  val instanceIdx: Gen[Any, InstanceIdx] = Gen.int.map(InstanceIdx.fromInt)

  val outerAliasKind: Gen[Any, OuterAliasKind] = Gen.oneOf(
    Gen.const(OuterAliasKind.CoreModule),
    Gen.const(OuterAliasKind.Type),
    Gen.const(OuterAliasKind.CoreType),
    Gen.const(OuterAliasKind.Component)
  )

  val componentExternalKind: Gen[Any, ComponentExternalKind] = Gen.oneOf(
    Gen.const(ComponentExternalKind.Module),
    Gen.const(ComponentExternalKind.Func),
    Gen.const(ComponentExternalKind.Value),
    Gen.const(ComponentExternalKind.Type),
    Gen.const(ComponentExternalKind.Instance),
    Gen.const(ComponentExternalKind.Component)
  )

  val exportKind: Gen[Any, ExportKind] = Gen.oneOf(
    Gen.const(ExportKind.Func),
    Gen.const(ExportKind.Table),
    Gen.const(ExportKind.Mem),
    Gen.const(ExportKind.Global)
  )

  val aliasTarget: Gen[Any, AliasTarget] =
    for {
      ct  <- Gen.int
      idx <- Gen.int
    } yield AliasTarget(ct, idx)

  val alias: Gen[Any, Alias] =
    Gen.oneOf(
      for {
        kind <- componentExternalKind
        idx  <- instanceIdx
        name <- WasmAstGen.name
      } yield Alias.InstanceExport(kind, idx, name),
      for {
        kind <- exportKind
        idx  <- instanceIdx
        name <- WasmAstGen.name
      } yield Alias.CoreInstanceExport(kind, idx, name),
      for {
        kind   <- outerAliasKind
        target <- aliasTarget
      } yield Alias.Outer(kind, target)
    )

}
