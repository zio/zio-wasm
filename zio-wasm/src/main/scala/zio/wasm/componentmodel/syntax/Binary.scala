package zio.wasm.componentmodel.syntax

import zio.parser.*
import zio.prelude.*
import zio.wasm.componentmodel.*
import zio.wasm.syntax.{SyntaxError, Binary as WasmBinary}
import zio.Chunk
import zio.wasm.syntax.Binary.{funcIdx, memIdx, valType, vec1}
import zio.wasm.{Custom, Module, Url}

// TODO: simplify byte prefix based branching

object Binary {
  import WasmBinary.{
    BinarySyntax,
    BinaryReader,
    BinaryWriter,
    anyByte,
    anyBytes,
    specificByte,
    specificByte_,
    exportDesc,
    funcType,
    magic,
    name,
    Section,
    SectionId,
    section,
    u32,
    vec
  }

  private def optional[T](inner: BinarySyntax[T]): BinarySyntax[Option[T]] =
    specificByte(0x00)
      .transformEither(_ => Right(None), { case None => Right(0x00) })
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~ inner)
          .transformEither(i => Right(Some(i)), { case Some(i) => Right(i) })
          .widenWith(SyntaxError.InvalidCase)
      ) ?? "optional"

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
    (specificByte(0x00).unit(0x00) ~> WasmBinary.`import`)
      .of[ModuleDeclaration.Import]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~> WasmBinary.`type`).of[ModuleDeclaration.Type].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x02) ~> outerAliasKind ~ aliasTarget)
          .transformEither(
            { case (kind, target) =>
              Right(ModuleDeclaration.OuterAlias(Alias.Outer(kind, target)))
            },
            {
              case ModuleDeclaration.OuterAlias(Alias.Outer(kind, target)) => Right((kind, target))
              case _                                                       => Left(SyntaxError.InvalidCase)
            }
          )
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x03) ~ name ~ WasmBinary.exportDesc)
          .of[ModuleDeclaration.Export]
          .widenWith(
            SyntaxError.InvalidCase
          )
      )
      ?? "moduleDeclaration"

  private[wasm] val coreType: BinarySyntax[CoreType] =
    (specificByte_(0x60) ~> funcType)
      .of[CoreType.Function]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x50) ~> vec(moduleDeclaration)).of[CoreType.Module].widenWith(SyntaxError.InvalidCase)
      ) ?? "coreType"

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
    primitiveValueType
      .transformEither(
        pvt => Right(ComponentValType.Primitive(pvt)),
        {
          case ComponentValType.Primitive(pvt) => Right(pvt)
          case _                               => Left(SyntaxError.InvalidCase)
        }
      )
      .orElse(
        WasmBinary.typeIdx.transformEither(
          idx => Right(ComponentValType.Defined(idx)),
          {
            case ComponentValType.Defined(idx) => Right(idx)
            case _                             => Left(SyntaxError.InvalidCase)
          }
        )
      ) ?? "componentValType"

  private[wasm] val typeBounds: BinarySyntax[TypeBounds] =
    (specificByte_(0x00) ~> componentTypeIdx)
      .of[TypeBounds.Eq]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        specificByte(0x01).transformEither(
          _ => Right(TypeBounds.SubResource),
          {
            case TypeBounds.SubResource => Right(0x01)
            case _                      => Left(SyntaxError.InvalidCase)
          }
        )
      )
      ?? "typeBounds"

  private[wasm] val externDesc: BinarySyntax[ExternDesc] = {
    val parser =
      componentExternalKind.asParser.flatMap {
        case ComponentExternalKind.Module    => moduleIdx.asParser.to[ExternDesc.Module]
        case ComponentExternalKind.Func      => componentFuncIdx.asParser.to[ExternDesc.Func]
        case ComponentExternalKind.Value     => componentValType.asParser.to[ExternDesc.Val]
        case ComponentExternalKind.Type      => typeBounds.asParser.to[ExternDesc.Type]
        case ComponentExternalKind.Instance  => instanceIdx.asParser.to[ExternDesc.Instance]
        case ComponentExternalKind.Component => componentIdx.asParser.to[ExternDesc.Component]
      }

    val printer =
      Printer.byValue {
        case ExternDesc.Module(idx)    =>
          componentExternalKind.asPrinter(ComponentExternalKind.Module) ~ moduleIdx.asPrinter(idx)
        case ExternDesc.Func(idx)      =>
          componentExternalKind.asPrinter(ComponentExternalKind.Func) ~ componentFuncIdx.asPrinter(idx)
        case ExternDesc.Val(tpe)       =>
          componentExternalKind.asPrinter(ComponentExternalKind.Value) ~ componentValType.asPrinter(tpe)
        case ExternDesc.Type(bounds)   =>
          componentExternalKind.asPrinter(ComponentExternalKind.Type) ~ typeBounds.asPrinter(bounds)
        case ExternDesc.Instance(idx)  =>
          componentExternalKind.asPrinter(ComponentExternalKind.Instance) ~ instanceIdx.asPrinter(idx)
        case ExternDesc.Component(idx) =>
          componentExternalKind.asPrinter(ComponentExternalKind.Component) ~ componentIdx.asPrinter(idx)
      }

    (parser <=> printer) ?? "externDesc"
  }

  private[wasm] val instantiationArgRef: BinarySyntax[InstantiationArgRef] =
    (specificByte_(0x12) ~ instanceIdx).of[InstantiationArgRef.Instance].widenWith(SyntaxError.InvalidCase)
      ?? "instantiationArgRef"

  private[wasm] val instantiationArg: BinarySyntax[InstantiationArg] =
    (name ~ instantiationArgRef).of[InstantiationArg] ?? "instantiationArg"

  private[wasm] val instance: BinarySyntax[Instance] =
    (specificByte_(0x00) ~ moduleIdx ~ vec(instantiationArg))
      .of[Instance.Instantiate]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~ vec(WasmBinary.`export`)).of[Instance.FromExports].widenWith(SyntaxError.InvalidCase)
      ) ?? "instance"

  private[wasm] val url: BinarySyntax[Url] =
    vec(anyByte).transform(
      bytes => Url.fromBytes(bytes),
      name => name.toBytes
    ) ?? "name"

  private[wasm] val externName: BinarySyntax[ExternName] =
    (name ~ url).of[ExternName] ?? "externName"

  private[wasm] val componentExport: BinarySyntax[ComponentExport] =
    (externName ~ componentExternalKind ~ u32 ~ optional(externDesc)).of[ComponentExport] ?? "componentExport"

  private[wasm] val componentInstantiationArg: BinarySyntax[ComponentInstantiationArg] =
    (name ~ externDesc).of[ComponentInstantiationArg] ?? "componentInstantiationArg"

  private[wasm] val componentInstance: BinarySyntax[ComponentInstance] =
    (specificByte_(0x00) ~ componentIdx ~ vec(componentInstantiationArg))
      .of[ComponentInstance.Instantiate]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~ vec(componentExport))
          .of[ComponentInstance.FromExports]
          .widenWith(SyntaxError.InvalidCase)
      ) ?? "componentInstance"

  private[wasm] val variantCase: BinarySyntax[VariantCase] =
    (name ~ optional(componentValType) ~ optional(u32)).of[VariantCase] ?? "variantCase"

  private[wasm] val componentDefinedType: BinarySyntax[ComponentDefinedType] =
    primitiveValueType
      .transformEither(
        pvt => Right(ComponentDefinedType.Primitive(pvt)),
        { case ComponentDefinedType.Primitive(pvt) => Right(pvt); case _ => Left(SyntaxError.InvalidCase) }
      )
      .orElse(
        (specificByte_(0x72) ~ vec(name ~ componentValType))
          .of[ComponentDefinedType.Record]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x71) ~ vec(variantCase)).of[ComponentDefinedType.Variant].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x70) ~ componentValType).of[ComponentDefinedType.List].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x6f) ~ vec(componentValType)).of[ComponentDefinedType.Tuple].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x6e) ~ vec1(name)).of[ComponentDefinedType.Flags].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x6d) ~ vec1(name)).of[ComponentDefinedType.Enum].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x6c) ~ vec(componentValType)).of[ComponentDefinedType.Union].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x6b) ~ componentValType).of[ComponentDefinedType.Option].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x6a) ~ optional(componentValType) ~ optional(componentValType))
          .of[ComponentDefinedType.Result]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x69) ~ componentTypeIdx).of[ComponentDefinedType.Own].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x68) ~ componentTypeIdx).of[ComponentDefinedType.Borrow].widenWith(SyntaxError.InvalidCase)
      ) ?? "componentDefinedType"

  private[wasm] val componentFuncResult: BinarySyntax[ComponentFuncResult] =
    (specificByte_(0x00) ~ componentValType)
      .of[ComponentFuncResult.Unnamed]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~ vec(name ~ componentValType))
          .of[ComponentFuncResult.Named]
          .widenWith(SyntaxError.InvalidCase)
      ) ?? "componentFuncResult"

  private[wasm] val componentFuncType: BinarySyntax[ComponentFuncType] =
    (vec(name ~ componentValType) ~ componentFuncResult).of[ComponentFuncType] ?? "componentFuncType"

  private[wasm] val componentImport: BinarySyntax[ComponentImport] =
    (externName ~ externDesc).of[ComponentImport] ?? "componentImport"

  private[wasm] val componentTypeDeclaration: BinarySyntax[ComponentTypeDeclaration] =
    (specificByte_(0x00) ~ coreType)
      .of[ComponentTypeDeclaration.Core]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~ componentType).of[ComponentTypeDeclaration.Type].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x02) ~ alias).of[ComponentTypeDeclaration.Alias].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x03) ~ componentImport).of[ComponentTypeDeclaration.Import].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x04) ~ externName ~ externDesc)
          .of[ComponentTypeDeclaration.Export]
          .widenWith(SyntaxError.InvalidCase)
      ) ?? "componentTypeDeclaration"

  private[wasm] val instanceTypeDeclaration: BinarySyntax[InstanceTypeDeclaration] =
    (specificByte_(0x00) ~ coreType)
      .of[InstanceTypeDeclaration.Core]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~ componentType).of[InstanceTypeDeclaration.Type].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x02) ~ alias).of[InstanceTypeDeclaration.Alias].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x04) ~ externName ~ externDesc)
          .of[InstanceTypeDeclaration.Export]
          .widenWith(SyntaxError.InvalidCase)
      ) ?? "instanceTypeDeclaration"

  private[wasm] lazy val componentType: BinarySyntax[ComponentType] =
    (specificByte_(0x3f) ~ valType ~ optional(funcIdx))
      .of[ComponentType.Resource]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x40) ~ componentFuncType).of[ComponentType.Func].widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x41) ~ vec(componentTypeDeclaration))
          .of[ComponentType.Component]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x42) ~ vec(instanceTypeDeclaration))
          .of[ComponentType.Instance]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        componentDefinedType
          .of[ComponentType.Defined]
          .widenWith(SyntaxError.InvalidCase)
      )
      ?? "componentType"

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
    (specificByte_(0x00) ~ specificByte_(0x00) ~ funcIdx ~ vec(canonicalOption) ~ componentTypeIdx)
      .of[Canon.Lift]
      .widenWith(SyntaxError.InvalidCase)
      .orElse(
        (specificByte_(0x01) ~ specificByte_(0x00) ~ componentFuncIdx ~ vec(canonicalOption))
          .of[Canon.Lower]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x02) ~ componentTypeIdx)
          .of[Canon.ResourceNew]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x03) ~ componentValType)
          .of[Canon.ResourceDrop]
          .widenWith(SyntaxError.InvalidCase)
      )
      .orElse(
        (specificByte_(0x04) ~ componentTypeIdx)
          .of[Canon.ResourceRep]
          .widenWith(SyntaxError.InvalidCase)
      ) ?? "canon"

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

  private[wasm] object ComponentSection {
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

    def of[T](id: SectionId, syntax: BinarySyntax[T], value: T): Either[SyntaxError, Section] =
      syntax.print(value).map { rawContents =>
        Section(id, rawContents.size, rawContents)
      }
  }

  private val version =
    specificByte_(0x0c) ~>
      specificByte_(0x00) ~>
      specificByte_(0x01) ~>
      specificByte_(0x00)

  private def fromSections(sections: Chunk[Section]): Either[SyntaxError, Component] = {
    sections.groupBy(_.id).foreach { (id, sections) =>
      println(id.toString + " " + sections.map(_.size))
    }

    def section[T](id: SectionId, syntax: BinarySyntax[Chunk[T]]): Either[SyntaxError, Chunk[T]] =
      sections
        .filter(_.id == id)
        .map { section =>
          section.to(syntax)
        }
        .foldLeftM(Chunk.empty[Chunk[T]]) { case (r, s) =>
          s.map(r :+ _)
        }
        .map(_.flatten)

    def singleValueSection[T](id: SectionId, syntax: BinarySyntax[T]): Either[SyntaxError, Chunk[T]] =
      sections
        .filter(_.id == id)
        .map { section =>
          section.to(syntax)
        }
        .foldLeftM(Chunk.empty[T]) { case (r, s) =>
          s.map(r :+ _)
        }

    def customSections: Either[SyntaxError, Chunk[Custom]] =
      sections
        .filter(_.id == Section.custom)
        .map { section =>
          section.to(name ~ anyBytes).map { case (n, bs) =>
            Custom(n, bs)
          }
        }
        .foldLeftM(Chunk.empty[Custom]) { case (r, s) =>
          s.map(r :+ _)
        }

    for {
      modules            <- singleValueSection(ComponentSection.module, WasmBinary.module)
      coreInstances      <- section(ComponentSection.coreInstance, coreInstanceSection)
      coreTypes          <- section(ComponentSection.coreType, coreTypeSection)
      components         <- singleValueSection(ComponentSection.component, component)
      aliases            <- section(ComponentSection.alias, aliasSection)
      componentInstances <- section(ComponentSection.instance, instanceSection)
      types              <- section(ComponentSection.typ, typeSection)
      canons             <- section(ComponentSection.canon, canonSection)
      starts             <- singleValueSection(ComponentSection.start, componentStart)
      imports            <- section(ComponentSection.`import`, importSection)
      exports            <- section(ComponentSection.`export`, exportSection)
      custom             <- customSections
    } yield Component(
      modules = modules,
      instances = coreInstances,
      coreTypes = coreTypes,
      components = components,
      aliases = aliases,
      componentInstances = componentInstances,
      types = types,
      canons = canons,
      starts = starts,
      imports = imports,
      exports = exports,
      custom = custom
    )
  }

  private def toSections(component: Component): Either[SyntaxError, Chunk[Section]] = {
    def section[T](
        id: SectionId,
        syntax: BinarySyntax[T],
        value: => Chunk[T]
    ): Either[SyntaxError, Chunk[Section]] =
      value
        .map(Section.of(id, syntax, _))
        .foldLeftM(Chunk.empty[Section]) { case (r, s) =>
          s.map(r :+ _)
        }

    def customSection: Either[SyntaxError, Chunk[Section]] =
      component.custom
        .map { custom =>
          name.asPrinter.print(custom.name).map { nameBytes =>
            val data = nameBytes ++ custom.data
            Section(Section.custom, data.size, data)
          }
        }
        .foldLeftM(Chunk.empty[Section]) { case (r, s) =>
          s.map(r :+ _)
        }

    for {
      modules            <- section(ComponentSection.module, WasmBinary.module, component.modules)
      coreInstances      <- section(ComponentSection.coreInstance, instance, component.instances)
      coreTypes          <- section(ComponentSection.coreType, coreType, component.coreTypes)
      components         <- section(ComponentSection.component, Binary.component, component.components)
      aliases            <- section(ComponentSection.alias, alias, component.aliases)
      componentInstances <- section(ComponentSection.instance, componentInstance, component.componentInstances)
      types              <- section(ComponentSection.typ, componentType, component.types)
      canons             <- section(ComponentSection.canon, canon, component.canons)
      starts             <- section(ComponentSection.start, componentStart, component.starts)
      imports            <- section(ComponentSection.`import`, componentImport, component.imports)
      exports            <- section(ComponentSection.`export`, componentExport, component.exports)
      custom             <- customSection
    } yield modules ++ aliases ++ coreInstances ++ coreTypes ++ components ++ componentInstances ++ types ++ canons ++ starts ++ imports ++ exports ++ custom // TODO: preserve original order
  }

  private val componentValue: BinarySyntax[Component] =
    section.repeat0.transformEither(fromSections, toSections) ?? "component"

  lazy val component: BinarySyntax[Component] =
    (magic ~> version ~> componentValue) ?? "component file"
}
