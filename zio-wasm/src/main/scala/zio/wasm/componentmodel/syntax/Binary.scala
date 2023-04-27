package zio.wasm.componentmodel.syntax

import zio.parser.*
import zio.wasm.componentmodel.*
import zio.wasm.syntax.{SyntaxError, Binary as WasmBinary}
import zio.Chunk
import zio.wasm.Custom
import zio.wasm.syntax.Binary.anyByte

object Binary {
  import WasmBinary.{
    BinarySyntax,
    BinaryReader,
    BinaryWriter,
    anyBytes,
    specificByte,
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

  private[wasm] val instanceIdx: BinarySyntax[InstanceIdx] =
    u32.transform(
      InstanceIdx.fromInt,
      _.toInt
    ) ?? "instanceIdx"

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
//  private[wasm] val moduleDeclaration: BinarySyntax[ModuleDeclaration] =
//    (specificByte(0x00).unit(0x00) ~> name ~ exportDesc)
//      .of[ModuleDeclaration.Import]
//      .widenWith(SyntaxError.InvalidCase)
//      .orElse(
//        (specificByte(0x01).unit(0x01) ~> typ).of[ModuleDeclaration.Type].widenWith(SyntaxError.InvalidCase)
//      ) ?? "moduleDeclaration"

//  private[wasm] val coreType: BinarySyntax[CoreType] =
//    (specificByte(0x60).unit(0x60) ~> funcType)
//      .of[CoreType.Function]
//      .widenWith(SyntaxError.InvalidCase)
//      .orElse(
//        (specificByte(0x50).unit(0x50) ~> vec(moduleDeclaration)).of[CoreType.Module].widenWith(SyntaxError.InvalidCase)
//      ) ?? "coreType"

  private[wasm] val aliasSection: BinarySyntax[Chunk[Alias]] = vec(alias) ?? "aliasSection"

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
    specificByte(0x0c).unit(0x0c) ~>
      specificByte(0x00).unit(0x00) ~>
      specificByte(0x01).unit(0x01) ~>
      specificByte(0x00).unit(0x00)

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
        .foldLeft[Either[SyntaxError, Chunk[Chunk[T]]]](Right(Chunk.empty)) {
          case (Right(r), Right(section)) => Right(r :+ section)
          case (Left(err), _)             => Left(err)
          case (Right(_), Left(err))      => Left(err)
        }
        .map(_.flatten)

    def customSections: Either[SyntaxError, Chunk[Custom]] =
      sections
        .filter(_.id == Section.custom)
        .map { section =>
          section.to(name ~ anyBytes).map { case (n, bs) =>
            Custom(n, bs)
          }
        }
        .foldLeft[Either[SyntaxError, Chunk[Custom]]](Right(Chunk.empty)) {
          case (Right(r), Right(section)) => Right(r :+ section)
          case (Left(err), _)             => Left(err)
          case (Right(_), Left(err))      => Left(err)
        }

    for {
      aliases <- section(ComponentSection.alias, aliasSection)
      custom  <- customSections
    } yield Component(
      module = Chunk.empty,
      instances = Chunk.empty,
      coreTypes = Chunk.empty,
      component = Chunk.empty,
      aliases = aliases,
      types = Chunk.empty,
      canons = Chunk.empty,
      start = None,
      imports = Chunk.empty,
      exports = Chunk.empty,
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
        .foldLeft[Either[SyntaxError, Chunk[Section]]](Right(Chunk.empty)) {
          case (Right(r), Right(section)) => Right(r :+ section)
          case (Left(err), _)             => Left(err)
          case (Right(_), Left(err))      => Left(err)
        }

    def customSection: Either[SyntaxError, Chunk[Section]] =
      component.custom
        .map { custom =>
          name.asPrinter.print(custom.name).map { nameBytes =>
            val data = nameBytes ++ custom.data
            Section(Section.custom, data.size, data)
          }
        }
        .foldLeft[Either[SyntaxError, Chunk[Section]]](Right(Chunk.empty)) {
          case (Right(r), Right(section)) => Right(r :+ section)
          case (Left(err), _)             => Left(err)
          case (Right(_), Left(err))      => Left(err)
        }

    for {
      aliases <- section(ComponentSection.alias, alias, component.aliases)
      custom  <- customSection
    } yield aliases ++ custom
  }

  val component: BinarySyntax[Component] =
    (magic ~> version ~> section.repeat0).transformEither(fromSections, toSections) ?? "component"
}
