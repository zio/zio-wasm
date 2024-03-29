package zio.wasm.componentmodel.syntax

import zio.parser.*
import zio.prelude.*
import zio.wasm.componentmodel.*
import zio.wasm.syntax.{SyntaxError, Binary as WasmBinary}
import zio.Chunk
import zio.wasm.internal.BinarySyntax
import zio.wasm.internal.BinarySyntax.*
import zio.wasm.syntax.Binary.{funcIdx, memIdx, valType, vec1}
import zio.wasm.{Module, Section, SectionType, Sections, Url}

object Binary {
  import WasmBinary.{exportDesc, funcType, magic, name, RawSection, SectionId, section, u32, vec}

  private def optional[T](inner: BinarySyntax[T]): BinarySyntax[Option[T]] =
    casesByPrefix("optional")(
      Prefix(0x00) -> Syntax.succeed(None),
      Prefix(0x01) -> inner.of[Some[T]]
    )

  private[wasm] val componentIdx: BinarySyntax[ComponentIdx] =
    u32.transform(
      ComponentIdx.fromInt,
      _.toInt
    ) ?? "componentIdx"

  private[wasm] val instanceIdx: BinarySyntax[InstanceIdx] =
    u32.transform(
      InstanceIdx.fromInt,
      _.toInt
    ) ?? "instanceIdx"

  private[wasm] val valueIdx: BinarySyntax[ValueIdx] =
    u32.transform(
      ValueIdx.fromInt,
      _.toInt
    ) ?? "valueIdx"

  private[wasm] val moduleIdx: BinarySyntax[ModuleIdx] =
    u32.transform(
      ModuleIdx.fromInt,
      _.toInt
    ) ?? "moduleIdx"

  private[wasm] val componentFuncIdx: BinarySyntax[ComponentFuncIdx] =
    u32.transform(
      ComponentFuncIdx.fromInt,
      _.toInt
    ) ?? "componentFuncIdx"

  private[wasm] val componentTypeIdx: BinarySyntax[ComponentTypeIdx] =
    u32.transform(
      ComponentTypeIdx.fromInt,
      _.toInt
    ) ?? "componentTypeIdx"

  private[wasm] val outerAliasKind: BinarySyntax[OuterAliasKind] =
    ((specificByte(0x00) ~ specificByte(0x10)).transformTo(
      _ => OuterAliasKind.CoreType,
      { case OuterAliasKind.CoreType =>
        (0x00, 0x10)
      },
      SyntaxError.InvalidCase
    ) <>
      (specificByte(0x00) ~ specificByte(0x11)).transformTo(
        _ => OuterAliasKind.CoreModule,
        { case OuterAliasKind.CoreModule =>
          (0x00, 0x11)
        },
        SyntaxError.InvalidCase
      ) <>
      specificByte(0x03).transformTo(
        _ => OuterAliasKind.Type,
        { case OuterAliasKind.Type =>
          0x03
        },
        SyntaxError.InvalidCase
      ) <>
      specificByte(0x04).transformTo(
        _ => OuterAliasKind.Component,
        { case OuterAliasKind.Component =>
          0x04
        },
        SyntaxError.InvalidCase
      )) ?? "outerAliasKind"

  private[wasm] val componentExternalKind: BinarySyntax[ComponentExternalKind] =
    ((specificByte(0x00) ~ specificByte(0x11)).transformTo(
      _ => ComponentExternalKind.Module,
      { case ComponentExternalKind.Module =>
        (0x00, 0x11)
      },
      SyntaxError.InvalidCase
    ) <>
      specificByte(0x01).transformTo(
        _ => ComponentExternalKind.Func,
        { case ComponentExternalKind.Func =>
          0x01
        },
        SyntaxError.InvalidCase
      ) <>
      specificByte(0x02).transformTo(
        _ => ComponentExternalKind.Value,
        { case ComponentExternalKind.Value =>
          0x02
        },
        SyntaxError.InvalidCase
      ) <>
      specificByte(0x03).transformTo(
        _ => ComponentExternalKind.Type,
        { case ComponentExternalKind.Type =>
          0x03
        },
        SyntaxError.InvalidCase
      ) <>
      specificByte(0x04).transformTo(
        _ => ComponentExternalKind.Component,
        { case ComponentExternalKind.Component =>
          0x04
        },
        SyntaxError.InvalidCase
      ) <>
      specificByte(0x05).transformTo(
        _ => ComponentExternalKind.Instance,
        { case ComponentExternalKind.Instance =>
          0x05
        },
        SyntaxError.InvalidCase
      )) ?? "componentExternalKind"

  private[wasm] val exportKind: BinarySyntax[ExportKind] =
    (specificByte(0x00)
      .transformTo(
        _ => ExportKind.Func,
        { case ExportKind.Func => 0x00 },
        SyntaxError.InvalidCase
      ) <>
      specificByte(0x01)
        .transformTo(
          _ => ExportKind.Table,
          { case ExportKind.Table => 0x01 },
          SyntaxError.InvalidCase
        ) <>
      specificByte(0x02)
        .transformTo(
          _ => ExportKind.Mem,
          { case ExportKind.Mem => 0x02 },
          SyntaxError.InvalidCase
        ) <>
      specificByte(0x03)
        .transformTo(
          _ => ExportKind.Global,
          { case ExportKind.Global => 0x03 },
          SyntaxError.InvalidCase
        )) ?? "exportKind"

  private[wasm] val aliasTarget: BinarySyntax[AliasTarget] =
    (u32 ~ u32).of[AliasTarget] ?? "aliasTarget"

  private[wasm] val alias: BinarySyntax[Alias] = {
    val parser: BinaryReader[Alias] =
      for {
        b0    <- anyByte.asParser
        b1    <- if (b0 == 0x00) anyByte.asParser.map(Some(_)) else Parser.succeed(None)
        b3    <- anyByte.asParser
        alias <- b3 match {
                   case 0x00    =>
                     componentExternalKind.asParser.parseChunk(Chunk(b0) ++ Chunk.fromIterable(b1)) match {
                       case Right(kind) =>
                         (instanceIdx ~ name).asParser.map { case (idx, name) => Alias.InstanceExport(kind, idx, name) }
                       case Left(error) => Parser.fail(SyntaxError.InnerParserError(error))
                     }
                   case 0x01    =>
                     exportKind.asParser.parseChunk(Chunk.fromIterable(b1)) match {
                       case Right(kind) =>
                         (instanceIdx ~ name).asParser.map { case (idx, name) =>
                           Alias.CoreInstanceExport(kind, idx, name)
                         }
                       case Left(error) => Parser.fail(SyntaxError.InnerParserError(error))
                     }
                   case 0x02    =>
                     outerAliasKind.asParser.parseChunk(Chunk(b0) ++ Chunk.fromIterable(b1)) match {
                       case Right(kind) =>
                         aliasTarget.asParser.map(target => Alias.Outer(kind, target))
                       case Left(error) => Parser.fail(SyntaxError.InnerParserError(error))
                     }
                   case _: Byte => Parser.fail(SyntaxError.InvalidCase)
                 }
      } yield alias

    val printer: BinaryWriter[Alias] =
      Printer.byValue {
        case Alias.InstanceExport(kind, idx, n)     =>
          componentExternalKind.asPrinter(kind) ~ anyByte.asPrinter(0x00) ~ instanceIdx.asPrinter(idx) ~ name.asPrinter(
            n
          )
        case Alias.CoreInstanceExport(kind, idx, n) =>
          anyByte.asPrinter(0x00) ~ exportKind.asPrinter(kind) ~ anyByte.asPrinter(0x01) ~ instanceIdx.asPrinter(
            idx
          ) ~ name.asPrinter(n)
        case Alias.Outer(kind, target)              =>
          outerAliasKind.asPrinter(kind) ~ anyByte.asPrinter(0x02) ~ aliasTarget.asPrinter(target)
      }

    (parser <=> printer) ?? "alias"
  }

  private[wasm] val moduleDeclaration: BinarySyntax[ModuleDeclaration] =
    casesByPrefix("moduleDeclaration")(
      Prefix(0x00) -> WasmBinary.`import`.of[ModuleDeclaration.Import],
      Prefix(0x01) -> WasmBinary.`type`.of[ModuleDeclaration.Type],
      Prefix(0x02) -> (outerAliasKind ~ aliasTarget).of[Alias.Outer].of[ModuleDeclaration.OuterAlias],
      Prefix(0x03) -> (name ~ WasmBinary.exportDesc).of[ModuleDeclaration.Export]
    )

  private[wasm] val coreType: BinarySyntax[CoreType] =
    casesByPrefix("coreType")(
      Prefix(0x60) -> funcType.of[CoreType.Function],
      Prefix(0x50) -> vec(moduleDeclaration).of[CoreType.Module]
    )

  private[wasm] val primitiveValueType: BinarySyntax[PrimitiveValueType] =
    anyByte.transformEither(
      {
        case 0x7f => Right(PrimitiveValueType.Bool)
        case 0x7e => Right(PrimitiveValueType.S8)
        case 0x7d => Right(PrimitiveValueType.U8)
        case 0x7c => Right(PrimitiveValueType.S16)
        case 0x7b => Right(PrimitiveValueType.U16)
        case 0x7a => Right(PrimitiveValueType.S32)
        case 0x79 => Right(PrimitiveValueType.U32)
        case 0x78 => Right(PrimitiveValueType.S64)
        case 0x77 => Right(PrimitiveValueType.U64)
        case 0x76 => Right(PrimitiveValueType.F32)
        case 0x75 => Right(PrimitiveValueType.F64)
        case 0x74 => Right(PrimitiveValueType.Chr)
        case 0x73 => Right(PrimitiveValueType.Str)
        case _    => Left(SyntaxError.InvalidCase)
      },
      {
        case PrimitiveValueType.Bool => Right(0x7f)
        case PrimitiveValueType.S8   => Right(0x7e)
        case PrimitiveValueType.U8   => Right(0x7d)
        case PrimitiveValueType.S16  => Right(0x7c)
        case PrimitiveValueType.U16  => Right(0x7b)
        case PrimitiveValueType.S32  => Right(0x7a)
        case PrimitiveValueType.U32  => Right(0x79)
        case PrimitiveValueType.S64  => Right(0x78)
        case PrimitiveValueType.U64  => Right(0x77)
        case PrimitiveValueType.F32  => Right(0x76)
        case PrimitiveValueType.F64  => Right(0x75)
        case PrimitiveValueType.Chr  => Right(0x74)
        case PrimitiveValueType.Str  => Right(0x73)
      }
    ) ?? "primitiveValueType"

  private[wasm] val componentValType: BinarySyntax[ComponentValType] =
    casesByPrefix("componentValType")(
      Prefix.None -> primitiveValueType.of[ComponentValType.Primitive],
      Prefix.None -> componentTypeIdx.of[ComponentValType.Defined]
    )

  private[wasm] val typeBounds: BinarySyntax[TypeBound] =
    casesByPrefix("typeBound")(
      Prefix(0x00) -> componentTypeIdx.of[TypeBound.Eq],
      Prefix(0x01) -> Syntax.succeed(TypeBound.SubResource)
    )

  private[wasm] val externDesc: BinarySyntax[ExternDesc] = {
    val parser =
      componentExternalKind.asParser.flatMap {
        case ComponentExternalKind.Module    => componentTypeIdx.asParser.to[ExternDesc.Module]
        case ComponentExternalKind.Func      => componentTypeIdx.asParser.to[ExternDesc.Func]
        case ComponentExternalKind.Value     => componentValType.asParser.to[ExternDesc.Val]
        case ComponentExternalKind.Type      => typeBounds.asParser.to[ExternDesc.Type]
        case ComponentExternalKind.Instance  => componentTypeIdx.asParser.to[ExternDesc.Instance]
        case ComponentExternalKind.Component => componentTypeIdx.asParser.to[ExternDesc.Component]
      }

    val printer =
      Printer.byValue {
        case ExternDesc.Module(idx)    =>
          componentExternalKind.asPrinter(ComponentExternalKind.Module) ~ componentTypeIdx.asPrinter(idx)
        case ExternDesc.Func(idx)      =>
          componentExternalKind.asPrinter(ComponentExternalKind.Func) ~ componentTypeIdx.asPrinter(idx)
        case ExternDesc.Val(tpe)       =>
          componentExternalKind.asPrinter(ComponentExternalKind.Value) ~ componentValType.asPrinter(tpe)
        case ExternDesc.Type(bounds)   =>
          componentExternalKind.asPrinter(ComponentExternalKind.Type) ~ typeBounds.asPrinter(bounds)
        case ExternDesc.Instance(idx)  =>
          componentExternalKind.asPrinter(ComponentExternalKind.Instance) ~ componentTypeIdx.asPrinter(idx)
        case ExternDesc.Component(idx) =>
          componentExternalKind.asPrinter(ComponentExternalKind.Component) ~ componentTypeIdx.asPrinter(idx)
      }

    (parser <=> printer) ?? "externDesc"
  }

  private[wasm] val instantiationArgRef: BinarySyntax[InstantiationArgRef] =
    (specificByte_(0x12) ~ instanceIdx).of[InstantiationArgRef.Instance].widenWith(SyntaxError.InvalidCase)
      ?? "instantiationArgRef"

  private[wasm] val instantiationArg: BinarySyntax[InstantiationArg] =
    (name ~ instantiationArgRef).of[InstantiationArg] ?? "instantiationArg"

  private[wasm] val instance: BinarySyntax[Instance] =
    casesByPrefix("instance")(
      Prefix(0x00) -> (moduleIdx ~ vec(instantiationArg)).of[Instance.Instantiate],
      Prefix(0x01) -> vec(WasmBinary.`export`).of[Instance.FromExports]
    )

  private[wasm] val url: BinarySyntax[Url] =
    vec(anyByte).transform(
      bytes => Url.fromBytes(bytes),
      name => name.toBytes
    ) ?? "name"

  private[wasm] val componentExternName: BinarySyntax[ComponentExternName] =
    casesByPrefix("componentExternName")(
      Prefix(0x00) -> name.of[ComponentExternName.Kebab],
      Prefix(0x01) -> name.of[ComponentExternName.Interface]
    )

  private[wasm] val componentExport: BinarySyntax[ComponentExport] =
    (componentExternName ~ componentExternalKind ~ u32 ~ optional(externDesc)).of[ComponentExport] ?? "componentExport"

  private[wasm] val componentInstantiationArg: BinarySyntax[ComponentInstantiationArg] =
    (name ~ componentExternalKind ~ u32).of[ComponentInstantiationArg] ?? "componentInstantiationArg"

  private[wasm] val componentInstance: BinarySyntax[ComponentInstance] =
    casesByPrefix("componentInstance")(
      Prefix(0x00) -> (componentIdx ~ vec(componentInstantiationArg)).of[ComponentInstance.Instantiate],
      Prefix(0x01) -> vec(componentExport).of[ComponentInstance.FromExports]
    )

  private[wasm] val variantCase: BinarySyntax[VariantCase] =
    (name ~ optional(componentValType) ~ optional(u32)).of[VariantCase] ?? "variantCase"

  private[wasm] val componentDefinedType: BinarySyntax[ComponentDefinedType] =
    casesByPrefix("componentDefinedType")(
      Prefix.None  -> primitiveValueType.of[ComponentDefinedType.Primitive],
      Prefix(0x72) -> (vec(name ~ componentValType)).of[ComponentDefinedType.Record],
      Prefix(0x71) -> (vec(variantCase)).of[ComponentDefinedType.Variant],
      Prefix(0x70) -> componentValType.of[ComponentDefinedType.List],
      Prefix(0x6f) -> (vec(componentValType)).of[ComponentDefinedType.Tuple],
      Prefix(0x6e) -> (vec1(name)).of[ComponentDefinedType.Flags],
      Prefix(0x6d) -> (vec1(name)).of[ComponentDefinedType.Enum],
      Prefix(0x6c) -> (vec(componentValType)).of[ComponentDefinedType.Union],
      Prefix(0x6b) -> componentValType.of[ComponentDefinedType.Option],
      Prefix(0x6a) -> (optional(componentValType) ~ optional(componentValType)).of[ComponentDefinedType.Result],
      Prefix(0x69) -> componentTypeIdx.of[ComponentDefinedType.Own],
      Prefix(0x68) -> componentTypeIdx.of[ComponentDefinedType.Borrow]
    )

  private[wasm] val componentFuncResult: BinarySyntax[ComponentFuncResult] =
    casesByPrefix("componentFuncResult")(
      Prefix(0x00) -> componentValType.of[ComponentFuncResult.Unnamed],
      Prefix(0x01) -> (vec(name ~ componentValType)).of[ComponentFuncResult.Named]
    )

  private[wasm] val componentFuncType: BinarySyntax[ComponentFuncType] =
    (vec(name ~ componentValType) ~ componentFuncResult).of[ComponentFuncType] ?? "componentFuncType"

  private[wasm] val componentImport: BinarySyntax[ComponentImport] =
    (componentExternName ~ externDesc).of[ComponentImport] ?? "componentImport"

  private[wasm] val componentTypeDeclaration: BinarySyntax[ComponentTypeDeclaration] =
    casesByPrefix("componentTypeDeclaration")(
      Prefix(0x00) -> coreType.of[ComponentTypeDeclaration.Core],
      Prefix(0x01) -> componentType.of[ComponentTypeDeclaration.Type],
      Prefix(0x02) -> alias.of[ComponentTypeDeclaration.Alias],
      Prefix(0x03) -> componentImport.of[ComponentTypeDeclaration.Import],
      Prefix(0x04) -> (componentExternName ~ externDesc).of[ComponentTypeDeclaration.Export]
    )

  private[wasm] val instanceTypeDeclaration: BinarySyntax[InstanceTypeDeclaration] =
    casesByPrefix("instanceTypeDeclaration")(
      Prefix(0x00) -> coreType.of[InstanceTypeDeclaration.Core],
      Prefix(0x01) -> componentType.of[InstanceTypeDeclaration.Type],
      Prefix(0x02) -> alias.of[InstanceTypeDeclaration.Alias],
      Prefix(0x04) -> (componentExternName ~ externDesc).of[InstanceTypeDeclaration.Export]
    )

  private[wasm] lazy val componentType: BinarySyntax[ComponentType] =
    casesByPrefix("componentType")(
      Prefix(0x3f) -> (valType ~ optional(componentFuncIdx)).of[ComponentType.Resource],
      Prefix(0x40) -> componentFuncType.of[ComponentType.Func],
      Prefix(0x41) -> vec(componentTypeDeclaration).of[ComponentType.Component],
      Prefix(0x42) -> vec(instanceTypeDeclaration).of[ComponentType.Instance],
      Prefix.None  -> componentDefinedType.of[ComponentType.Defined]
    )

  private[wasm] val canonicalOption: BinarySyntax[CanonicalOption] =
    specificByte_(0x00)
      .transformEither(
        _ => Right(CanonicalOption.Utf8),
        {
          case CanonicalOption.Utf8 => Right(())
          case _                    => Left(SyntaxError.InvalidCase)
        }
      )
      .orElse(
        specificByte_(0x01)
          .transformEither(
            _ => Right(CanonicalOption.Utf16),
            {
              case CanonicalOption.Utf16 => Right(())
              case _                     => Left(SyntaxError.InvalidCase)
            }
          )
      )
      .orElse(
        specificByte_(0x02)
          .transformEither(
            _ => Right(CanonicalOption.CompactUtf16),
            {
              case CanonicalOption.CompactUtf16 => Right(())
              case _                            => Left(SyntaxError.InvalidCase)
            }
          )
      )
      .orElse(
        (specificByte_(0x03) ~ memIdx)
          .of[CanonicalOption.Memory]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x04) ~ funcIdx)
          .of[CanonicalOption.Realloc]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x05) ~ funcIdx)
          .of[CanonicalOption.PostReturn]
          .widenWith(SyntaxError.InvalidCase)
      ) ?? "canonicalOption"

  private[wasm] val canon: BinarySyntax[Canon] =
    casesByPrefix("canon")(
      Prefix(0x00, 0x00) -> (funcIdx ~ vec(canonicalOption) ~ componentTypeIdx).of[Canon.Lift],
      Prefix(0x01, 0x00) -> (componentFuncIdx ~ vec(canonicalOption)).of[Canon.Lower],
      Prefix(0x02)       -> componentTypeIdx.of[Canon.ResourceNew],
      Prefix(0x03)       -> componentValType.of[Canon.ResourceDrop],
      Prefix(0x04)       -> componentTypeIdx.of[Canon.ResourceRep]
    )

  private[wasm] val componentStart: BinarySyntax[ComponentStart] =
    (componentFuncIdx ~ vec(valueIdx) ~ u32).of[ComponentStart] ?? "componentStart"

  private[wasm] val aliasSection: BinarySyntax[Chunk[Alias]]                = vec(alias) ?? "aliasSection"
  private[wasm] val coreTypeSection: BinarySyntax[Chunk[CoreType]]          = vec(coreType) ?? "coreTypeSection"
  private[wasm] val coreInstanceSection: BinarySyntax[Chunk[Instance]]      = vec(instance) ?? "coreInstanceSection"
  private[wasm] val instanceSection: BinarySyntax[Chunk[ComponentInstance]] =
    vec(componentInstance) ?? "instanceSection"
  private[wasm] val typeSection: BinarySyntax[Chunk[ComponentType]]         = vec(componentType) ?? "typeSection"
  private[wasm] val canonSection: BinarySyntax[Chunk[Canon]]                = vec(canon) ?? "canonSection"
  private[wasm] val importSection: BinarySyntax[Chunk[ComponentImport]]     = vec(componentImport) ?? "importSection"
  private[wasm] val exportSection: BinarySyntax[Chunk[ComponentExport]]     = vec(componentExport) ?? "exportSection"

  private[wasm] object RawComponentSection {
    val custom: SectionId       = SectionId.fromByte(0)
    val module: SectionId       = SectionId.fromByte(1)
    val coreInstance: SectionId = SectionId.fromByte(2)
    val coreType: SectionId     = SectionId.fromByte(3)
    val component: SectionId    = SectionId.fromByte(4)
    val instance: SectionId     = SectionId.fromByte(5)
    val alias: SectionId        = SectionId.fromByte(6)
    val typ: SectionId          = SectionId.fromByte(7)
    val canon: SectionId        = SectionId.fromByte(8)
    val start: SectionId        = SectionId.fromByte(9)
    val `import`: SectionId     = SectionId.fromByte(10)
    val `export`: SectionId     = SectionId.fromByte(11)

    def of[T](id: SectionId, syntax: BinarySyntax[T], value: T): Either[SyntaxError, RawSection] =
      syntax.print(value).map { rawContents =>
        RawSection(id, rawContents.size, rawContents)
      }
  }

  private[wasm] val version =
    (anyByte <~
      specificByte_(0x00) ~>
      specificByte_(0x01) ~>
      specificByte_(0x00)).transformEither(
      byte =>
        if (byte == 0x0d) Right(())
        else if (byte < 0x0d) Left(SyntaxError.OldComponentVersion(byte))
        else Left(SyntaxError.NewComponentVersion(byte)),
      _ => Right(0x0d)
    )

  private def fromSections(sections: Chunk[RawSection]): Either[SyntaxError, Component] = {
    def loadSection(section: RawSection): Either[SyntaxError, Chunk[Section[ComponentIndexSpace]]] =
      section.id match {
        case RawComponentSection.module       => section.to(WasmBinary.module).map(Chunk.single)
        case RawComponentSection.coreInstance => section.to(coreInstanceSection)
        case RawComponentSection.coreType     => section.to(coreTypeSection)
        case RawComponentSection.component    => section.to(component).map(Chunk.single)
        case RawComponentSection.instance     => section.to(instanceSection)
        case RawComponentSection.alias        => section.to(aliasSection)
        case RawComponentSection.typ          => section.to(typeSection)
        case RawComponentSection.canon        => section.to(canonSection)
        case RawComponentSection.start        => section.to(componentStart).map(Chunk.single)
        case RawComponentSection.`import`     => section.to(importSection)
        case RawComponentSection.`export`     => section.to(exportSection)
        case _                                => section.to(name ~ anyBytes).map { case (n, bs) => Chunk(Custom(n, bs)) }
      }

    sections.forEach(loadSection).map { sections =>
      Component(Sections.fromGrouped(sections))
    }
  }

  private def toSections(component: Component): Either[SyntaxError, Chunk[RawSection]] = {
    def encodeGroup(
        sectionType: SectionType[ComponentIndexSpace],
        sections: Chunk[Section[ComponentIndexSpace]]
    ): Either[SyntaxError, RawSection] =
      sectionType match {
        case ComponentSectionType.ComponentTypeSection         =>
          RawSection.of(sectionType.binaryId, typeSection, sections.asInstanceOf[Chunk[ComponentType]])
        case ComponentSectionType.ComponentStartSection        =>
          RawSection.of(sectionType.binaryId, componentStart, sections.head.asInstanceOf[ComponentStart])
        case ComponentSectionType.ComponentExportSection       =>
          RawSection.of(sectionType.binaryId, exportSection, sections.asInstanceOf[Chunk[ComponentExport]])
        case ComponentSectionType.ComponentSection             =>
          RawSection.of(sectionType.binaryId, Binary.component, sections.head.asInstanceOf[Component])
        case ComponentSectionType.ComponentCoreInstanceSection =>
          RawSection.of(sectionType.binaryId, coreInstanceSection, sections.asInstanceOf[Chunk[Instance]])
        case ComponentSectionType.ComponentInstanceSection     =>
          RawSection.of(sectionType.binaryId, instanceSection, sections.asInstanceOf[Chunk[ComponentInstance]])
        case ComponentSectionType.ComponentModuleSection       =>
          RawSection.of(sectionType.binaryId, WasmBinary.module, sections.head.asInstanceOf[Module])
        case ComponentSectionType.ComponentCoreTypeSection     =>
          RawSection.of(sectionType.binaryId, coreTypeSection, sections.asInstanceOf[Chunk[CoreType]])
        case ComponentSectionType.ComponentAliasSection        =>
          RawSection.of(sectionType.binaryId, aliasSection, sections.asInstanceOf[Chunk[Alias]])
        case ComponentSectionType.ComponentCanonSection        =>
          RawSection.of(sectionType.binaryId, canonSection, sections.asInstanceOf[Chunk[Canon]])
        case ComponentSectionType.ComponentImportSection       =>
          RawSection.of(sectionType.binaryId, importSection, sections.asInstanceOf[Chunk[ComponentImport]])
        case ComponentSectionType.ComponentCustomSection       =>
          val custom = sections.head.asInstanceOf[Custom]
          name.asPrinter.print(custom.name).map { nameBytes =>
            val data = nameBytes ++ custom.data
            RawSection(RawSection.custom, data.size, data)
          }
      }

    component.sections.toGrouped.forEach { case (sectionType, sections) => encodeGroup(sectionType, sections) }
  }

  private val componentValue: BinarySyntax[Component] =
    section.repeat0.transformEither(fromSections, toSections) ?? "component"

  lazy val component: BinarySyntax[Component] =
    (magic ~> version ~> componentValue) ?? "component file"
}
