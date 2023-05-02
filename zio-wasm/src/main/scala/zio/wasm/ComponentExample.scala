package zio.wasm

import io.github.vigoo.prox.*
import io.github.vigoo.prox.zstream.*
import zio.*
import zio.nio.file.*
import zio.prelude.fx.*
import zio.stream.*
import zio.wasm.componentmodel.*
import zio.wasm.componentmodel.syntax.Binary

object ComponentExample extends ZIOAppDefault {

  final case class MoveOutTypeResult(outerTypeIdx: ComponentTypeIdx, additionalTypes: Set[ComponentTypeIdx])

  def moveAllTypesFromInnerComponent(
      innerComponentIdx: ComponentIdx,
      types: Set[ComponentTypeIdx]
  ): ComponentBuilder[Map[ComponentTypeIdx, ComponentTypeIdx]] = {
    val orderedTypes = Chunk.fromIterable(types)
    ZPure
      .foreach(orderedTypes) { componentTypeIdx =>
        moveTypeFromInnerComponent(innerComponentIdx, componentTypeIdx)
      }
      .flatMap { results =>
        val additionalTypes = results.map(_.additionalTypes).toSet.flatten.diff(types)
        val resultMapping   = orderedTypes.zip(results.map(_.outerTypeIdx)).toMap
        if (additionalTypes.nonEmpty)
          moveAllTypesFromInnerComponent(innerComponentIdx, additionalTypes).map(_ ++ resultMapping)
        else
          ZPure.succeed(resultMapping)
      }
  }

  def externDescTypeIdx(desc: ExternDesc): Option[ComponentTypeIdx] =
    desc match {
      case ExternDesc.Module(typeIdx)    => Some(typeIdx)
      case ExternDesc.Func(typeIdx)      => Some(typeIdx)
      case ExternDesc.Val(valType)       => None
      case ExternDesc.Type(typeBounds)   => None
      case ExternDesc.Instance(typeIdx)  => Some(typeIdx)
      case ExternDesc.Component(typeIdx) => Some(typeIdx)
    }

  def moveTypeFromInnerComponent(
      innerComponentIdx: ComponentIdx,
      innerTypeIdx: ComponentTypeIdx
  ): ComponentBuilder[MoveOutTypeResult] = {
    import ComponentBuilder.*

    // Get the type definition from the inner component
    // Add it to the outer component
    // Replace it with an outer alias in the inner component

    for {
      innerComponent       <- getComponent(innerComponentIdx)
      innerType             = innerComponent.getComponentType(innerTypeIdx).get
      // TODO: We need to replace the IDs in the inner section to the already known "fake ones" in order to maintain the dependency graph
      outerTypeIdx         <-
        innerType match {
          case ct: ComponentType   =>
            addComponentType(ct)
          case ci: ComponentImport =>
            addComponentImport(ci).flatMap {
              case SectionReference.ComponentType(typeIdx) => ZPure.succeed(typeIdx)
              case _                                       => ZPure.fail("Unexpected import type, not importing a component type")
            }
          case alias: Alias        =>
            println(s"Adding alias: $alias")
            addAlias(alias).flatMap {
              case SectionReference.ComponentType(typeIdx) => ZPure.succeed(typeIdx)
              case _                                       => ZPure.fail("Unexpected alias type, not refering to a component type")
            }
        }
      _                    <- modifyComponent(innerComponentIdx) {
                                for {
                                  _ <- replaceComponentType(
                                         innerTypeIdx,
                                         Alias.Outer(OuterAliasKind.Type, AliasTarget(1, outerTypeIdx.toInt))
                                       )
                                } yield ()
                              }
      _                    <-
        log(
          s"Inner type $innerTypeIdx moved to outer type $outerTypeIdx and replaced with an outer alias"
        )
      innerTypeOuterAliases = innerType match {
                                case ct: ComponentType =>
                                  ct.collectAliases().collect {
                                    case (level, outer: Alias.Outer) if outer.target.ct == level =>
                                      outer
                                  }
                                case _                 => Chunk.empty
                              }
      additionalTypes      <- if (innerTypeOuterAliases.isEmpty) ZPure.succeed(Set.empty)
                              else
                                // The type definition we moved contains at least one Alias.Outer.
                                // These can refer to any level depending on their position, but maximum to the original inner
                                // component as it was top-level.
                                //
                                // We only need to check those that were referring to the inner component and
                                // copy the original referenced types recursively to the new outer component.
                                // (using moveTypeFromInnerComponent)
                                for {
                                  _              <- log(s"It has outer aliases: $innerTypeOuterAliases")
                                  additionalTypes = innerTypeOuterAliases.map { outerAlias =>
                                                      // We are assuming here that the outer alias referes to a component type.
                                                      // I think this is always the case when being in a component type definition
                                                      ComponentTypeIdx.fromInt(outerAlias.target.idx)
                                                    }.toSet
                                } yield additionalTypes
    } yield MoveOutTypeResult(outerTypeIdx, additionalTypes)
  }

  def wrapComponent(component: Component): ZIO[Any, String, Component] =
    ComponentBuilder.runOn(Component.empty) {
      import ComponentBuilder.*

      for {
        componentIdx <- addComponent(component)
        _            <- log(s"Added the original component as $componentIdx")

        _ <- addManyComponentTypes {
               for {
                 // Copying all component imports of the inner component
                 results        <- ZPure.foreach(component.imports) { componentImport =>
                                     val oldId = component.sectionReference(componentImport)
                                     addComponentImport(componentImport).map { newId =>
                                       (oldId, newId, externDescTypeIdx(componentImport.desc).toList.toSet)
                                     }
                                   }
                 oldImportIds    = results.map(_._1)
                 importIds       = results.map(_._2)
                 additionalTypes = results.flatMap(_._3).toSet
                 _              <- log(s"Added ${importIds.size} imports: ${importIds} (previously: ${oldImportIds}")

                 // The types in additionalTypes also have to be copied to top level as they are referenced by the imports or
                 // aliases in the already copied types.
                 additionalIds <- moveAllTypesFromInnerComponent(componentIdx, additionalTypes)
                 _             <- log(s"Moved ${additionalIds.size} additional types to the outer component: ${additionalIds}")
                 // At this point all outer aliases pointing to the top level in the the added types must be one of those that
                 // has been moved, so we have to update them:
                 _             <- modifyAllComponentTypes { ct =>
                                    ct.mapAliases {
                                      case (level, outer: Alias.Outer) if outer.target.ct == level =>
                                        outer.copy(target =
                                          AliasTarget(level, additionalIds(ComponentTypeIdx.fromInt(outer.target.idx)).toInt)
                                        )
                                      case (_, other: Alias)                                       =>
                                        other
                                    }
                                  }
                 // And also we have to update refernces to the moved imports and types
                 mapper         = SectionReference.Mapper.fromPairs(
                                    oldImportIds.zip(importIds) ++
                                      Chunk.fromIterable(additionalIds).map { case (oldTypeIdx, newTypeIdx) =>
                                        SectionReference.ComponentType(oldTypeIdx) -> SectionReference.ComponentType(newTypeIdx)
                                      }
                                  )
                 _             <- mapAllSectionReference(mapper)
               } yield ()
             }
        // Copying all component exports of the inner component
//        exportIds <- ZPure.foreach(component.exports) { componentExport =>
//                       addComponentExport(componentExport)
//                     }
//        _         <- log(s"Added ${exportIds.size} exports: ${exportIds}")

        // Moving the final inner component to the end, because it needs to be able to refer to outer types
        _ <- moveToEnd(componentIdx)
      } yield ()
    }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Unit] = {
    implicit val runner: ProcessRunner[JVMProcessInfo] = new JVMProcessRunner

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
      _ <- ZIO.debug(s"Component functions:\n${component.funcIndex.mkString("\n")}")

      _ <- ZIO.debug(
             component.imports.head.desc match {
               case ExternDesc.Instance(typeIdx) =>
                 s"\n\nFirst import is importing instance of type ${component.getComponentType(typeIdx)}"
             }
           )

      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(0)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(2)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(3)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(7)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(8)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(9)))
      _ <- ZIO.debug(component.getComponentType(ComponentTypeIdx.fromInt(10)))

      wrapped  <- wrapComponent(component)
      bytes2   <- ZIO.fromEither(Binary.component.print(wrapped))
      _        <- Files.writeBytes(Path("examples/example_component_1_wrapped.wasm"), bytes2)
      toWat     = proc"wasm-tools print examples/example_component_1_wrapped.wasm" > Path(
                    "examples/example_component_1_wrapped.wat"
                  ).toFile.toPath
      watBytes <- toWat.run()
    } yield ()
  }
}
