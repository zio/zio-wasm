package zio.wasm.tools

import zio.*
import zio.nio.file.*
import zio.parser.*
import zio.wasm.syntax.Binary as WasmBinary
import zio.wasm.componentmodel.syntax.Binary

object ExtractCoreModules extends ZIOAppDefault {

  def run =
    for {
      args             <- ZIO.service[ZIOAppArgs]
      inputFile         = Path(args.getArgs(0))
      outputDir         = inputFile.parent
      bytes            <- Files.readAllBytes(inputFile)
      parser            = WasmBinary.magic ~> Binary.version ~> WasmBinary.section.repeat0
      sections         <- ZIO.fromEither(parser.parseChunk(bytes))
      moduleSections    = sections.filter(_.id == Binary.RawComponentSection.module)
      componentSections = sections.filter(_.id == Binary.RawComponentSection.component)
      _                <- ZIO.debug(s"Having ${moduleSections.length} modules on top level")
      _                <- ZIO.debug(s"Having ${componentSections.length} components on top level")
      _                <- ZIO.foreachDiscard(moduleSections.zipWithIndex) { case (section, index) =>
                            for {
                              _         <- ZIO.debug(s"Saving module $index")
                              outputName = "module" + index + ".wasm"
                              outputPath = outputDir.map(_ / outputName).getOrElse(Path(outputName))
                              _         <- Files.writeBytes(outputPath, section.rawContents)
                            } yield ()
                          }
      _                <- ZIO.foreachDiscard(componentSections.zipWithIndex) { case (section, index) =>
                            for {
                              _         <- ZIO.debug(s"Saving component $index")
                              outputName = "component" + index + ".wasm"
                              outputPath = outputDir.map(_ / outputName).getOrElse(Path(outputName))
                              _         <- Files.writeBytes(outputPath, section.rawContents)
                            } yield ()
                          }

    } yield ()
}
