package zio.wasm

import zio.*
import zio.nio.file.*
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
    } yield ()
}
