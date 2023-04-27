package zio.wasm.componentmodel.syntax

import zio.ZIO
import zio.test.{Spec, ZIOSpecDefault, assertTrue, check}
import zio.wasm.componentmodel.AstGen
import zio.wasm.syntax.BinarySpec.suite

object BinarySpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Any] =
    suite("Component Model Binary syntax")(
      test("alias") {
        check(AstGen.alias) { alias =>
          for {
            bytes  <- ZIO.fromEither(Binary.alias.print(alias))
            result <- ZIO.fromEither(Binary.alias.parseChunk(bytes))
          } yield assertTrue(result == alias)
        }
      }
    )
}
