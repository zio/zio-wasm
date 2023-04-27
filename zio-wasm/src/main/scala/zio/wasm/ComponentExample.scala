package zio.wasm

import zio.*
import zio.nio.file.*
import zio.wasm.componentmodel.syntax.Binary

object ComponentExample extends ZIOAppDefault {
  override def run: ZIO[ZIOAppArgs & Scope, Any, Unit] =
    for {
      bytes     <- Files.readAllBytes(Path("examples/example_component_1.wasm"))
      component <- ZIO.fromEither(Binary.component.parseChunk(bytes))
      _         <- ZIO.debug(component)
    } yield ()
}
