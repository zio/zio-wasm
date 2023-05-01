package zio.wasm.componentmodel

import zio.*
import zio.prelude.*
import zio.prelude.fx.*
import zio.wasm.ExportIdx
import zio.wasm.componentmodel.ComponentBuilder.State.CapturingComponentTypes

type ComponentBuilder[A] = ZPure[String, ComponentBuilder.State, ComponentBuilder.State, Any, String, A]
object ComponentBuilder {

  def addComponent(component: Component): ComponentBuilder[ComponentIdx] =
    ZPure.modify(_.modifyComponent(_.addComponent(component)))

  def addComponentImport(componentImport: ComponentImport): ComponentBuilder[ComponentTypeIdx | ValueIdx] =
    ZPure.modify(_.modifyComponent(_.addComponentImport(componentImport)))

  def addComponentExport(componentExport: ComponentExport): ComponentBuilder[ExportIdx] =
    ZPure.modify(_.modifyComponent(_.addComponentExport(componentExport)))

  def addComponentType(
      componentType: ComponentType | ComponentImport | Alias,
      insertionPoint: InsertionPoint
  ): ComponentBuilder[ComponentTypeIdx] =
    ZPure.get[State].flatMap {
      case State.Normal(_)                                                                      =>
        insertionPoint match {
          case InsertionPoint.LastOfType  => ZPure.modify(_.modifyComponent(_.addComponentType(componentType)))
          case InsertionPoint.FirstOfType =>
            ZPure.modify(_.modifyComponent(_.insertComponentTypeToBeginning(componentType)))
        }
      case State.CapturingComponentTypes(component, firstFakeId, nextFakeId, newComponentTypes) =>
        ZPure
          .set(
            State.CapturingComponentTypes(component, firstFakeId, nextFakeId.next, newComponentTypes :+ componentType)
          )
          .as(nextFakeId)
    }

  /** Captures addComponentType in the inner block and only add them in the end, reordering and reindexing them based on
    * their dependencies.
    */
  def addManyComponentTypes[A](inner: ComponentBuilder[A]): ComponentBuilder[A] =
    ZPure.get[State].flatMap {
      case State.Normal(component)                   =>
        val firstFakeId = ComponentTypeIdx.fromInt(1000000)
        ZPure.set(State.CapturingComponentTypes(component, firstFakeId, firstFakeId, Chunk.empty)) *>
          inner <*
          finishAddingManyComponentTypes
      case State.CapturingComponentTypes(_, _, _, _) =>
        ZPure.fail("Cannot nest addManyComponentTypes")
    }

  private def finishAddingManyComponentTypes: ComponentBuilder[Unit] =
    ZPure.get[State].flatMap {
      case State.CapturingComponentTypes(component, firstFakeId, _, newComponentTypes) if newComponentTypes.nonEmpty =>
        // TODO: Figure out the required topology
        ZPure.set(State.Normal(component)) *>
          ZPure.foreachDiscard(newComponentTypes) { componentType =>
            addComponentType(componentType, InsertionPoint.LastOfType)
          }

      case _ =>
        ZPure.unit
    }

  def getComponent(componentIdx: ComponentIdx): ComponentBuilder[Component] =
    ZPure.get[State].map(_.component.getComponent(componentIdx)).flatMap {
      case Some(component) => ZPure.succeed(component)
      case None            => ZPure.fail(s"Component $componentIdx not found")
    }

  def modifyAllComponentTypes(f: ComponentType => ComponentType): ComponentBuilder[Unit] =
    ZPure.get[State].flatMap {
      case State.Normal(_)                                                                      =>
        ZPure.update(_.mapComponent(_.mapAllComponentTypes(f)))
      case State.CapturingComponentTypes(component, firstFakeId, nextFakeId, newComponentTypes) =>
        ZPure.set(
          State.CapturingComponentTypes(
            component.mapAllComponentTypes(f),
            firstFakeId,
            nextFakeId,
            newComponentTypes.map {
              case componentType: ComponentType => f(componentType)
              case other                        => other
            }
          )
        )
    }

  def modifyComponent(componentIdx: ComponentIdx)(builder: ComponentBuilder[Unit]): ComponentBuilder[Unit] =
    getComponent(componentIdx).flatMap { component =>
      embed(component)(builder).flatMap { modifiedComponent =>
        replaceComponent(componentIdx, modifiedComponent)
      }
    }

  def log(msg: String): ComponentBuilder[Unit] =
    ZPure.log(msg)

  def moveToEnd(componentIdx: ComponentIdx): ComponentBuilder[Unit] =
    ZPure.update(_.mapComponent(_.moveToEnd(componentIdx)))

  def replaceComponent(componentIdx: ComponentIdx, newComponent: Component): ComponentBuilder[Unit] =
    ZPure.update(_.mapComponent(_.replaceComponent(componentIdx, newComponent)))

  def replaceComponentType(
      componentTypeIdx: ComponentTypeIdx,
      newComponentType: ComponentType
  ): ComponentBuilder[Unit] =
    ZPure.update(_.mapComponent(_.replaceComponentType(componentTypeIdx, newComponentType)))

  def replaceComponentType(
      componentTypeIdx: ComponentTypeIdx,
      alias: Alias
  ): ComponentBuilder[Unit] =
    ZPure.update(_.mapComponent(_.replaceComponentType(componentTypeIdx, alias)))

  def runOn(initial: Component)(builder: ComponentBuilder[Unit]): ZIO[Any, String, Component] = {
    val (log, result) = builder.runAll(State(initial))
    ZIO.foreachDiscard(log)(msg => ZIO.debug(msg)).zipRight {
      ZIO.fromEither(result).mapError(_.first).map(_._1.component)
    }
  }

  def embed(initial: Component)(builder: ComponentBuilder[Unit]): ComponentBuilder[Component] = {
    val (log, result) = builder.runAll(State(initial))
    ZPure.foreachDiscard(log)(msg => ZPure.log(msg)).zipRight {
      ZPure.fromEither(result).mapError(_.first).map(_._1.component)
    }
  }

  enum State {
    case Normal(component: Component)
    case CapturingComponentTypes(
        component: Component,
        firstFakeId: ComponentTypeIdx,
        nextFakeId: ComponentTypeIdx,
        newComponentTypes: Chunk[ComponentType | ComponentImport | Alias]
    )

    def component: Component

    def mapComponent(f: Component => Component): State =
      this match {
        case Normal(component)                                                              =>
          Normal(f(component))
        case CapturingComponentTypes(component, firstFakeId, nextFakeId, newComponentTypes) =>
          CapturingComponentTypes(f(component), firstFakeId, nextFakeId, newComponentTypes)
      }

    def modifyComponent[A](f: Component => (A, Component)): (A, State) = {
      val (result, newComponent) = f(component)
      (
        result,
        this match {
          case Normal(component)                                                              =>
            Normal(newComponent)
          case CapturingComponentTypes(component, firstFakeId, nextFakeId, newComponentTypes) =>
            CapturingComponentTypes(newComponent, firstFakeId, nextFakeId, newComponentTypes)
        }
      )
    }
  }

  object State {
    def apply(component: Component): State = State.Normal(component)
  }

  enum InsertionPoint {
    case FirstOfType
    case LastOfType
  }
}
