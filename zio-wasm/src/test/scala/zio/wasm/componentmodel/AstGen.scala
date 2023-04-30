package zio.wasm.componentmodel

import zio.Chunk
import zio.test.Gen
import zio.wasm.{Sections, AstGen as WasmAstGen}

object AstGen {
  val componentIdx: Gen[Any, ComponentIdx]         = Gen.int.map(ComponentIdx.fromInt)
  val instanceIdx: Gen[Any, InstanceIdx]           = Gen.int.map(InstanceIdx.fromInt)
  val componentTypeIdx: Gen[Any, ComponentTypeIdx] = Gen.int.map(ComponentTypeIdx.fromInt)

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

  val moduleDeclaration: Gen[Any, ModuleDeclaration] =
    Gen.oneOf(
      WasmAstGen.funcType.map(ft => ModuleDeclaration.Type(zio.wasm.Type.Func(ft))),
      for {
        name       <- WasmAstGen.name
        exportDesc <- WasmAstGen.exportDesc
      } yield ModuleDeclaration.Export(name, exportDesc),
      WasmAstGen.`import`.map(ModuleDeclaration.Import.apply),
      for {
        kind   <- outerAliasKind
        target <- aliasTarget
      } yield ModuleDeclaration.OuterAlias(Alias.Outer(kind, target))
    )

  val coreType: Gen[Any, CoreType] =
    Gen.oneOf(
      WasmAstGen.funcType.map(CoreType.Function.apply),
      Gen.chunkOf(moduleDeclaration).map(CoreType.Module.apply)
    )

  val moduleIdx: Gen[Any, ModuleIdx] =
    Gen.int.map(ModuleIdx.fromInt)

  val componentFuncIdx: Gen[Any, ComponentFuncIdx] =
    Gen.int.map(ComponentFuncIdx.fromInt)

  val valueIdx: Gen[Any, ValueIdx] =
    Gen.int.map(ValueIdx.fromInt)

  val instantiationArgRef: Gen[Any, InstantiationArgRef] =
    Gen.oneOf(
      instanceIdx.map(InstantiationArgRef.Instance.apply)
    )

  val instantiationArg: Gen[Any, InstantiationArg] =
    for {
      n   <- WasmAstGen.name
      ref <- instantiationArgRef
    } yield InstantiationArg(n, ref)

  val externName: Gen[Any, ExternName] =
    for {
      n <- WasmAstGen.name
      u <- WasmAstGen.url
    } yield ExternName(n, u)

  val primitiveValType: Gen[Any, PrimitiveValueType] =
    Gen.oneOf(
      Gen.const(PrimitiveValueType.Bool),
      Gen.const(PrimitiveValueType.S8),
      Gen.const(PrimitiveValueType.U8),
      Gen.const(PrimitiveValueType.S16),
      Gen.const(PrimitiveValueType.U16),
      Gen.const(PrimitiveValueType.S32),
      Gen.const(PrimitiveValueType.U32),
      Gen.const(PrimitiveValueType.S64),
      Gen.const(PrimitiveValueType.U64),
      Gen.const(PrimitiveValueType.F32),
      Gen.const(PrimitiveValueType.F64),
      Gen.const(PrimitiveValueType.Chr),
      Gen.const(PrimitiveValueType.Str)
    )

  val componentValType: Gen[Any, ComponentValType] =
    Gen.oneOf(
      primitiveValType.map(ComponentValType.Primitive.apply),
      WasmAstGen.typeIdx.map(ComponentValType.Defined.apply)
    )

  val typeBounds: Gen[Any, TypeBounds] =
    Gen.oneOf(
      componentTypeIdx.map(TypeBounds.Eq.apply),
      Gen.const(TypeBounds.SubResource)
    )

  val externDesc: Gen[Any, ExternDesc] =
    Gen.oneOf(
      moduleIdx.map(ExternDesc.Module.apply),
      componentFuncIdx.map(ExternDesc.Func.apply),
      componentValType.map(ExternDesc.Val.apply),
      typeBounds.map(ExternDesc.Type.apply),
      instanceIdx.map(ExternDesc.Instance.apply),
      componentIdx.map(ExternDesc.Component.apply)
    )

  val componentExport: Gen[Any, ComponentExport] =
    for {
      externName <- externName
      kind       <- componentExternalKind
      idx        <- Gen.int
      desc       <- Gen.option(externDesc)
    } yield ComponentExport(externName, kind, idx, desc)

  val instance: Gen[Any, Instance] =
    Gen.oneOf(
      for {
        idx  <- moduleIdx
        args <- Gen.chunkOf(instantiationArg)
      } yield Instance.Instantiate(idx, args),
      Gen.chunkOf(WasmAstGen.`export`).map(Instance.FromExports.apply)
    )

  val componentInstantiationArg: Gen[Any, ComponentInstantiationArg] =
    for {
      n    <- WasmAstGen.name
      desc <- externDesc
    } yield ComponentInstantiationArg(n, desc)

  val componentInstance: Gen[Any, ComponentInstance] =
    Gen.oneOf(
      for {
        idx  <- componentIdx
        args <- Gen.chunkOf(componentInstantiationArg)
      } yield ComponentInstance.Instantiate(idx, args),
      Gen.chunkOf(componentExport).map(ComponentInstance.FromExports.apply)
    )

  val variantCase: Gen[Any, VariantCase] =
    for {
      n       <- WasmAstGen.name
      t       <- Gen.option(componentValType)
      refines <- Gen.option(Gen.int)
    } yield VariantCase(n, t, refines)

  val componentDefinedType: Gen[Any, ComponentDefinedType] =
    Gen.oneOf(
      primitiveValType.map(ComponentDefinedType.Primitive.apply),
      Gen.chunkOf(WasmAstGen.name <*> componentValType).map(ComponentDefinedType.Record.apply),
      Gen.chunkOf(variantCase).map(ComponentDefinedType.Variant.apply),
      componentValType.map(ComponentDefinedType.List.apply),
      Gen.chunkOf(componentValType).map(ComponentDefinedType.Tuple.apply),
      Gen.chunkOf1(WasmAstGen.name).map(ComponentDefinedType.Flags.apply),
      Gen.chunkOf1(WasmAstGen.name).map(ComponentDefinedType.Enum.apply),
      Gen.chunkOf(componentValType).map(ComponentDefinedType.Union.apply),
      componentValType.map(ComponentDefinedType.Option.apply),
      (Gen.option(componentValType) <*> Gen.option(componentValType)).map(ComponentDefinedType.Result.apply),
      componentTypeIdx.map(ComponentDefinedType.Own.apply),
      componentTypeIdx.map(ComponentDefinedType.Borrow.apply)
    )

  val componentFuncResult: Gen[Any, ComponentFuncResult] =
    Gen.oneOf(
      componentValType.map(ComponentFuncResult.Unnamed.apply),
      Gen.chunkOf(WasmAstGen.name <*> componentValType).map(ComponentFuncResult.Named.apply)
    )

  val componentFuncType: Gen[Any, ComponentFuncType] =
    for {
      params <- Gen.chunkOf(WasmAstGen.name <*> componentValType)
      result <- componentFuncResult
    } yield ComponentFuncType(params, result)

  val componentImport: Gen[Any, ComponentImport] =
    (externName <*> externDesc).map(ComponentImport.apply)

  val componentTypeDeclaration: Gen[Any, ComponentTypeDeclaration] =
    Gen.oneOf(
      coreType.map(ComponentTypeDeclaration.Core.apply),
      Gen.suspend(componentType.map(ComponentTypeDeclaration.Type.apply)),
      alias.map(ComponentTypeDeclaration.Alias.apply),
      componentImport.map(ComponentTypeDeclaration.Import.apply),
      (externName <*> externDesc).map(ComponentTypeDeclaration.Export.apply)
    )

  val instanceTypeDeclaration: Gen[Any, InstanceTypeDeclaration] =
    Gen.oneOf(
      coreType.map(InstanceTypeDeclaration.Core.apply),
      Gen.suspend(componentType.map(InstanceTypeDeclaration.Type.apply)),
      alias.map(InstanceTypeDeclaration.Alias.apply),
      (externName <*> externDesc).map(InstanceTypeDeclaration.Export.apply)
    )

  lazy val componentType: Gen[Any, ComponentType] =
    Gen.oneOf(
      componentDefinedType.map(ComponentType.Defined.apply),
      componentFuncType.map(ComponentType.Func.apply),
      Gen.chunkOf(componentTypeDeclaration).map(ComponentType.Component.apply),
      Gen.chunkOf(instanceTypeDeclaration).map(ComponentType.Instance.apply),
      (WasmAstGen.valType <*> Gen.option(WasmAstGen.funcIdx)).map(ComponentType.Resource.apply)
    )

  val canonicalOption: Gen[Any, CanonicalOption] =
    Gen.oneOf(
      Gen.const(CanonicalOption.Utf8),
      Gen.const(CanonicalOption.Utf16),
      Gen.const(CanonicalOption.CompactUtf16),
      WasmAstGen.memIdx.map(CanonicalOption.Memory.apply),
      WasmAstGen.funcIdx.map(CanonicalOption.Realloc.apply),
      WasmAstGen.funcIdx.map(CanonicalOption.PostReturn.apply)
    )

  val canon: Gen[Any, Canon] =
    Gen.oneOf(
      for {
        funcIdx  <- WasmAstGen.funcIdx
        opts     <- Gen.chunkOf(canonicalOption)
        funcType <- componentTypeIdx
      } yield Canon.Lift(funcIdx, opts, funcType),
      for {
        funcIdx <- componentFuncIdx
        opts    <- Gen.chunkOf(canonicalOption)
      } yield Canon.Lower(funcIdx, opts),
      componentTypeIdx.map(Canon.ResourceNew.apply),
      componentValType.map(Canon.ResourceDrop.apply),
      componentTypeIdx.map(Canon.ResourceRep.apply)
    )

  val componentStart: Gen[Any, ComponentStart] =
    for {
      funcIdx <- componentFuncIdx
      args    <- Gen.chunkOf(valueIdx)
      results <- Gen.int
    } yield ComponentStart(funcIdx, args, results)

  private lazy val componentFlat: Gen[Any, Component] =
    Gen.suspend {
      for {
        modules            <- Gen.chunkOf(WasmAstGen.module)
        instances          <- Gen.chunkOf(instance)
        coreTypes          <- Gen.chunkOf(coreType)
        components          = Chunk.empty
        componentInstances <- Gen.chunkOf(componentInstance)
        aliases            <- Gen.chunkOf(alias)
        types              <- Gen.chunkOf(componentType)
        canons             <- Gen.chunkOf(canon)
        starts             <- Gen.chunkOf(componentStart)
        imports            <- Gen.chunkOf(componentImport)
        exports            <- Gen.chunkOf(componentExport)
        custom              = Chunk.empty
      } yield Component(
        Sections.fromGrouped(
          Chunk(
            modules,
            instances,
            coreTypes,
            components,
            componentInstances,
            aliases,
            types,
            canons,
            starts,
            imports,
            exports,
            custom
          )
        )
      )
    }

  lazy val component: Gen[Any, Component] =
    Gen.suspend {
      for {
        modules            <- Gen.chunkOf(WasmAstGen.module)
        instances          <- Gen.chunkOf(instance)
        coreTypes          <- Gen.chunkOf(coreType)
        components         <- Gen.chunkOf(componentFlat)
        componentInstances <- Gen.chunkOf(componentInstance)
        aliases            <- Gen.chunkOf(alias)
        types              <- Gen.chunkOf(componentType)
        canons             <- Gen.chunkOf(canon)
        starts             <- Gen.chunkOf(componentStart)
        imports            <- Gen.chunkOf(componentImport)
        exports            <- Gen.chunkOf(componentExport)
        custom              = Chunk.empty
      } yield Component(
        Sections.fromGrouped(
          Chunk(
            modules,
            instances,
            coreTypes,
            components,
            componentInstances,
            aliases,
            types,
            canons,
            starts,
            imports,
            exports,
            custom
          )
        )
      )
    }
}
