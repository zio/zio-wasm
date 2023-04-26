package zio.wasm

import zio.nio.file.{Files, Path}
import zio.wasm.syntax.Binary
import zio.{Chunk, ZIO, ZIOAppDefault}

object Example extends ZIOAppDefault {
  override def run: ZIO[Any, Any, Any] =
    for {
      bytes   <- Files.readAllBytes(Path("examples/wasm_game_of_life_bg.wasm"))
      module  <- ZIO.fromEither(Binary.module.parseChunk(bytes))
//      _       <- ZIO.debug(module.toString)
      bytes2  <- ZIO.fromEither(Binary.module.print(module))
      _       <- Files.writeBytes(Path("examples/wasm_game_of_life_bg_out.wasm"), bytes2)
      _       <- ZIO.debug(s"Serialized back into ${bytes2.length} bytes (original: ${bytes.length})")
      module2 <- ZIO.fromEither(Binary.module.parseChunk(bytes2))
//      _       <- ZIO.debug(module2.toString)
      _       <- ZIO.debug(s"Print/parse cycle equality: ${module == module2}")

      importedFunctions              = module.importedFunctions
      _                             <- ZIO.debug("Imported functions")
      _                             <- ZIO.debug(importedFunctions.mkString("\n"))
      _                             <- ZIO.debug(s"First local function index: ${module.firstLocalFunctionIndex}")
      importedFunctionsWithCallCount = importedFunctions.map { case (funcIdx, (moduleName, name, funcType)) =>
                                         (
                                           funcIdx,
                                           (
                                             moduleName,
                                             name,
                                             funcType,
                                             module.foldLeftRec(0) {
                                               case (count, Instr.Call(fidx)) if fidx == funcIdx =>
                                                 count + 1
                                               case (count, _)                                   =>
                                                 count
                                             }
                                           )
                                         )
                                       }
      _                             <- ZIO.debug(importedFunctionsWithCallCount.mkString("\n"))

      // NOTE: this example transformation results in invalid WASM because we don't have the parameters on stack for the second call
//      module3 = module.mapInstr {
//        case Instr.Call(fidx) if importedFunctions.keySet.contains(fidx) =>
//          Instr.Block(BlockType.None, Chunk(Instr.Call(fidx), Instr.Call(fidx)))
//        case i: Instr => i
//      }
//      bytes3 <- ZIO.fromEither(Binary.module.print(module3))
//      _ <- Files.writeBytes(Path("examples/wasm_game_of_life_bg_call_imported_twice.wasm"), bytes3)

      allCallIndirects = module.foldLeftRec(Chunk.empty[Instr.CallIndirect]) {
                           case (result, i @ Instr.CallIndirect(_, _)) => result :+ i
                           case (result, _)                            => result
                         }
      _               <- ZIO.debug(s"All indirect calls:\n${allCallIndirects.mkString("\n")}")
      _               <- ZIO.debug(s"Tables:\n${module.tables.mkString("\n")}")
    } yield ()

    /*
    NOTES:
      To wrap a Call we need to inject a function which gets exactly the same parameters as the wrapped one,
      then loads all parameters from local to stack, and calls the original function.

      We need an equivalent wrapper function for each wrapped function

      Then we also need to take care of indirect
      function calls. To do so, we need to generate a wrapper per function type, which is static part of the operation.

      Then in these wrappers we need to first load the function index from the the first local, then
      put back the rest of the local parameters to stack.
      Then we load the function reference from the table pointed by the first local.
      We map that to the wrapped function index generated for direct calls, and set this function reference in
      a new, fresh table (added by the transformation).
      Then we call indirectly using this new table.
     */
}
