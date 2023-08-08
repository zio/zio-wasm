package zio.wasm

import zio.*
import zio.nio.file.*
import zio.prelude.fx.*
import zio.stream.*
import zio.wasm.componentmodel.*
import zio.wasm.componentmodel.syntax.Binary
import zio.wasm.metadata.*

object ComponentExample2 extends ZIOAppDefault {

  override def run: ZIO[ZIOAppArgs & Scope, Any, Unit] =
    for {
      bytes     <- Files.readAllBytes(Path("integration-tests/src/it/wasm/clocks.wasm"))
      component <- ZIO.fromEither(Binary.component.parseChunk(bytes))
      _         <- ZIO.debug(s"Component loaded")
      bytes2    <- ZIO.fromEither(Binary.component.print(component))
      producers  = component.allProducerSections
      _         <- ZIO.debug(producers)
//      _          <- Files.writeBytes(Path("examples/main_out.wasm"), bytes2)
//      component2 <- ZIO.fromEither(Binary.component.parseChunk(bytes2))
//      _          <- ZIO.debug(s"Re-read component is the same: ${component == component2}")
//      _          <- ZIO.debug(s"Rewrote binary is the same: ${bytes == bytes2}")

    } yield ()
}
