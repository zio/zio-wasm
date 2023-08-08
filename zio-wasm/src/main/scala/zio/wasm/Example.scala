package zio.wasm

import zio.nio.file.{Files, Path}
import zio.wasm.syntax.Binary
import zio.{Chunk, ChunkBuilder, ZIO, ZIOAppDefault}

import scala.collection.mutable

object Example extends ZIOAppDefault {

  def wrapFunctionCall(
    originalIdx: FuncIdx,
    funcType: FuncType,
    moduleName: Option[Name],
    functionName: Option[Name]
  ): Option[Expr] =
    if (moduleName.isDefined && functionName.isDefined) {
      // imported function
      val loadParameters = Chunk.fromIterable(funcType.input.values.indices).map { idx =>
        Instr.LocalGet(LocalIdx.fromInt(idx))
      }
      val callFunction   = Chunk(Instr.Call(originalIdx))

      Some {
        Expr(loadParameters ++ callFunction)
      }
    } else {
      // not wrapping local functions
      None
    }

  def wrapFunctions(module: Module, wrap: (FuncIdx, FuncType, Option[Name], Option[Name]) => Option[Expr]): Module = {
    // Creating wrapper functions
    val originalFuncCount = module.lastFuncIdx.toInt + 1
    var nextFuncIdx       = FuncIdx.fromInt(originalFuncCount)
    var nextTypeIdx       = TypeIdx.fromInt(module.types.length)
    val newFuncs          = ChunkBuilder.make[Func]()
    val newTypes          = ChunkBuilder.make[FuncType]()
    val mapping           = mutable.Map[FuncIdx, FuncIdx]()
    val indirectMapping   = mutable.Map[Instr.CallIndirect, FuncIdx]()
    val mappingTableIdx   = TableIdx.fromInt(module.tables.length)

    for ((funcIdx, (moduleName, funcName, funcType)) <- module.importedFunctions)
      wrap(funcIdx, funcType, Some(moduleName), Some(funcName)) match {
        case Some(body) =>
          val locals = funcType.input.values
          newFuncs += Func(module.typeIdxOf(funcType), locals, body)
          mapping += (funcIdx -> nextFuncIdx)
          nextFuncIdx = nextFuncIdx.next
        case None       =>
      }

    for ((func, idx) <- module.funcs.zipWithIndexFrom(module.firstLocalFunctionIndex.toInt)) {
      val funcIdx  = FuncIdx.fromInt(idx)
      val funcType = module.types(func.typ.toInt)
      wrap(funcIdx, funcType, None, None) match {
        case Some(body) =>
          newFuncs += Func(func.typ, func.locals, body)
          mapping += (funcIdx -> nextFuncIdx)
          nextFuncIdx = nextFuncIdx.next
        case None       =>
      }
    }

    // Generating indirect invocation wrappers
    val allCallIndirects  = module.foldLeftRec(Chunk.empty[Instr.CallIndirect]) {
      case (result, i @ Instr.CallIndirect(_, _)) => result :+ i
      case (result, _)                            => result
    }
    val indirectCallTypes = allCallIndirects.distinct

    for (Instr.CallIndirect(tableIdx, typeIdx) <- indirectCallTypes) {
      val targetTyp = module.types(typeIdx.toInt)

      // The wrapper function's input type is extended with a function index
      val wrapperTyp = targetTyp.copy(
        input = targetTyp.input.copy(values = targetTyp.input.values :+ NumType.I32) // Also gets the function index
      )

      val existingWrapperTypIdx = module.typeIdxOf(wrapperTyp)
      val wrapperTypIdx         =
        if (existingWrapperTypIdx.toInt == -1) {
          val idx = nextTypeIdx
          nextTypeIdx = nextTypeIdx.next
          newTypes += wrapperTyp
          idx
        } else {
          existingWrapperTypIdx
        }

      val locals = wrapperTyp.input.values

      // Loading each function parameter and pushing them to the stack
      val loadParameters = Chunk.fromIterable(wrapperTyp.input.values.indices).map { idx =>
        Instr.LocalGet(LocalIdx.fromInt(idx))
      }

      val body = Expr(
        loadParameters ++ Chunk(
          Instr.TableGet(tableIdx),                    // get the unwrapped function's reference
          // TODO PROBLEM: problem: now we have funcref and we canot use it as a funcidx
          // only thing we can do with references: null, isNull, funcIdx->funcRef
          Instr.CallIndirect(mappingTableIdx, typeIdx) // get the wrapped referenced function idx and call it
        )
      )

      newFuncs += Func(
        wrapperTypIdx,
        locals,
        body
      )
      indirectMapping += (Instr.CallIndirect(tableIdx, typeIdx) -> nextFuncIdx)
      nextFuncIdx = nextFuncIdx.next
    }

    // Initializing the contents of the mapping table
    val initializer = Elem(
      RefType.FuncRef,
      Chunk.fromIterable(0 until originalFuncCount).map { i =>
        val from = FuncIdx.fromInt(i)
        val to   = mapping.getOrElse(from, from)
        Expr(Chunk(Instr.RefFunc(to)))
      },
      ElemMode.Active(mappingTableIdx, Expr(Chunk(Instr.I32Const(0))))
    )

    // Replacing call instructions
    module.mapInstr {
      case Instr.Call(funcIdx)   =>
        Instr.Call(mapping.getOrElse(funcIdx, funcIdx))
      case i: Instr.CallIndirect =>
        indirectMapping.get(i) match {
          case Some(funcIdx) =>
            Instr.Call(funcIdx)
          case None          =>
            i
        }
      case i: Instr              =>
        i
    }
      .addTypes(newTypes.result())
      .addFunctions(newFuncs.result())
      .addTable(Table(TableType(Limits(originalFuncCount, Some(originalFuncCount)), RefType.FuncRef)))
      ._1
      .addElem(initializer)
      ._1
      .mapExports {
        case Export(name, ExportDesc.Func(funcIdx)) =>
          Export(name, ExportDesc.Func(mapping.getOrElse(funcIdx, funcIdx)))
        case e: Export                              =>
          e
      }
  }

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

      _                             <- ZIO.debug(module.getFunction(FuncIdx.fromInt(1)).toString)
      _                             <- ZIO.debug(module.getFunction(FuncIdx.fromInt(10)).toString)
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
//                  case Instr.Call(fidx) if importedFunctions.keySet.contains(fidx) =>
//                    Instr.Block(BlockType.None, Chunk(Instr.Call(fidx), Instr.Call(fidx)))
//                  case i: Instr                                                    => i
//                }
//      bytes3 <- ZIO.fromEither(Binary.module.print(module3))
//      _      <- Files.writeBytes(Path("examples/wasm_game_of_life_bg_call_imported_twice.wasm"), bytes3)

      allCallIndirects = module.foldLeftRec(Chunk.empty[Instr.CallIndirect]) {
                           case (result, i @ Instr.CallIndirect(_, _)) => result :+ i
                           case (result, _)                            => result
                         }
      _               <- ZIO.debug(s"All indirect calls:\n${allCallIndirects.mkString("\n")}")
      _               <- ZIO.debug(s"Tables:\n${module.tables.mkString("\n")}")

      _ <- ZIO.debug(s"Original elems\n${module.elems.mkString("\n")}")

      module4 = wrapFunctions(module, wrapFunctionCall)
      bytes4 <- ZIO.fromEither(Binary.module.print(module4))
      _      <- Files.writeBytes(Path("examples/wasm_game_of_life_bg_fnwrap.wasm"), bytes4)
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
