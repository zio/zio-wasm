package zio.wasm

import zio.*
import zio.nio.file.*
import zio.prelude.fx.*
import zio.stream.*
import zio.wasm.componentmodel.*
import zio.wasm.componentmodel.syntax.Binary

object ComponentExample extends ZIOAppDefault {

  override def run: ZIO[ZIOAppArgs & Scope, Any, Unit] =
    for {
      bytes      <- Files.readAllBytes(Path("examples/example_component_1.wasm"))
      component  <- ZIO.fromEither(Binary.component.parseChunk(bytes))
      _          <- ZIO.debug(s"Component loaded")
      bytes2     <- ZIO.fromEither(Binary.component.print(component))
      _          <- Files.writeBytes(Path("examples/example_component_1_out.wasm"), bytes2)
      component2 <- ZIO.fromEither(Binary.component.parseChunk(bytes2))
      _          <- ZIO.debug(s"Re-read component is the same: ${component == component2}")
      _          <- ZIO.debug(s"Rewrote binary is the same: ${bytes == bytes2}")

      _ <- ZIO.debug(s"Imports:\n${component.imports.mkString("\n")}")
      _ <- ZIO.debug(s"Exports:\n${component.exports.mkString("\n")}")
      _ <- ZIO.debug(s"Core types:\n${component.coreTypes.mkString("\n")}")
      _ <- ZIO.debug(s"Canons:\n${component.canons.mkString("\n")}")
//      _ <- ZIO.debug(s"Core instances:\n${component.coreInstances.mkString("\n")}")
      _ <- ZIO.debug(s"Component instances:\n${component.instances.mkString("\n")}")

//      _ <- ZIO.debug(component.getCoreInstance(CoreInstanceIdx.fromInt(12)))

      _ <- ZIO.debug(s"Core functions:\n${component.coreFuncIndex.mkString("\n")}")
      _ <- ZIO.debug(s"Component functions:\n${component.componentFuncIndex.mkString("\n")}")

      _ <- ZIO.debug(
             component.imports.head.desc match {
               case ExternDesc.Instance(typeIdx) =>
                 s"\n\nFirst import is importing instance of type ${component.getComponentType(typeIdx)}"
               case _                            => "???"
             }
           )

      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(0)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(2)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(3)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(7)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(8)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(9)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(10)))

    } yield ()
}
