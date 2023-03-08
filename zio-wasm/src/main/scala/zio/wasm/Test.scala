package zio.wasm

import zio.nio.file.{Files, Path}
import zio.wasm.syntax.Binary
import zio.{ZIO, ZIOAppDefault}

object Test extends ZIOAppDefault {
  override def run: ZIO[Any, Any, Any] =
    for {
      bytes   <- Files.readAllBytes(Path("/home/vigoo/projects/oss/wasm_game_of_life/pkg/wasm_game_of_life_bg.wasm"))
      module  <- ZIO.fromEither(Binary.module.parseChunk(bytes))
      _       <- ZIO.debug(module.toString)
      bytes2  <- ZIO.fromEither(Binary.module.print(module))
      _       <- ZIO.debug(s"Serialized back into ${bytes2.length} bytes")
      module2 <- ZIO.fromEither(Binary.module.parseChunk(bytes2))
      _       <- ZIO.debug(module2.toString)
      _       <- ZIO.debug(module == module2)
    } yield ()
}
