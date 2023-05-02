package zio.wasm.componentmodel

import zio.*
import zio.prelude.*
import zio.prelude.fx.*
import zio.wasm.{ExportIdx, Section}

import scala.reflect.ClassTag

type ComponentBuilder[A] = ZPure[String, ComponentBuilder.State, ComponentBuilder.State, Any, String, A]
object ComponentBuilder {

  def addAlias(
      alias: Alias,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): ComponentBuilder[SectionReference] =
    ZPure.get[State].flatMap {
      case State.Normal(_) =>
        ZPure.modify(_.modifyComponent(_.addAlias(alias, insertionPoint)))

      case State.Capturing(component, firstFakeId, nextFakeId, newSections) =>
        (ComponentSectionType.ComponentAliasSection.indexSpace(alias) match {
          case ComponentIndexSpace.Component  =>
            ZPure.succeed(SectionReference.Component(ComponentIdx.fromInt(nextFakeId)))
          case ComponentIndexSpace.Instance   =>
            ZPure.succeed(SectionReference.Instance(InstanceIdx.fromInt(nextFakeId)))
          case ComponentIndexSpace.Module     =>
            ZPure.succeed(SectionReference.Module(ModuleIdx.fromInt(nextFakeId)))
          case ComponentIndexSpace.Type       =>
            ZPure.succeed(SectionReference.ComponentType(ComponentTypeIdx.fromInt(nextFakeId)))
          case ComponentIndexSpace.Func       =>
            ZPure.succeed(SectionReference.ComponentFunc(ComponentFuncIdx.fromInt(nextFakeId)))
          case ComponentIndexSpace.Value      => ZPure.succeed(SectionReference.Value(ValueIdx.fromInt(nextFakeId)))
          case ComponentIndexSpace.CoreType   => ??? // TODO
          case ComponentIndexSpace.CoreTable  => ??? // TODO
          case ComponentIndexSpace.CoreFunc   => ??? // TODO
          case ComponentIndexSpace.CoreGlobal => ??? // TODO
          case ComponentIndexSpace.CoreMem    => ??? // TODO
          case _                              => ZPure.fail("Unexpected index space for alias")
        }).flatMap { idx =>
          ZPure
            .set(
              State.Capturing(component, firstFakeId, nextFakeId + 1, newSections :+ (idx -> alias))
            )
            .as(idx)
        }
    }

  def addComponent(component: Component): ComponentBuilder[ComponentIdx] =
    ZPure.modify(_.modifyComponent(_.addComponent(component)))

  def addComponentImport(
      componentImport: ComponentImport,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): ComponentBuilder[SectionReference] =
    ZPure.get[State].flatMap {
      case State.Normal(_)                                                  =>
        ZPure.modify(_.modifyComponent(_.addComponentImport(componentImport, insertionPoint)))
      case State.Capturing(component, firstFakeId, nextFakeId, newSections) =>
        val idx = componentImport.desc match {
          case ExternDesc.Module(typeIdx)    => SectionReference.Module(ModuleIdx.fromInt(nextFakeId))
          case ExternDesc.Func(typeIdx)      => SectionReference.ComponentFunc(ComponentFuncIdx.fromInt(nextFakeId))
          case ExternDesc.Val(valType)       => SectionReference.Value(ValueIdx.fromInt(nextFakeId))
          case ExternDesc.Type(typeBounds)   => SectionReference.ComponentType(ComponentTypeIdx.fromInt(nextFakeId))
          case ExternDesc.Instance(typeIdx)  => SectionReference.Instance(InstanceIdx.fromInt(nextFakeId))
          case ExternDesc.Component(typeIdx) => SectionReference.Component(ComponentIdx.fromInt(nextFakeId))
        }
        ZPure
          .set(
            State.Capturing(component, firstFakeId, nextFakeId + 1, newSections :+ (idx -> componentImport))
          )
          .as(idx)
    }

  def addComponentExport(componentExport: ComponentExport): ComponentBuilder[ExportIdx] =
    ZPure.modify(_.modifyComponent(_.addComponentExport(componentExport)))

  def addComponentType(
      componentType: ComponentType,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): ComponentBuilder[ComponentTypeIdx] =
    ZPure.get[State].flatMap {
      case State.Normal(_) =>
        ZPure.modify(_.modifyComponent(_.addComponentType(componentType, insertionPoint)))

      case State.Capturing(component, firstFakeId, nextFakeId, newSections) =>
        val idx = ComponentTypeIdx.fromInt(nextFakeId)
        ZPure
          .set(
            State.Capturing(
              component,
              firstFakeId,
              nextFakeId + 1,
              newSections :+ (SectionReference.ComponentType(idx) -> componentType)
            )
          )
          .as(idx)
    }

  /** Captures addComponentType in the inner block and only add them in the end, reordering and reindexing them based on
    * their dependencies.
    */
  def addManyComponentTypes[A](inner: ComponentBuilder[A]): ComponentBuilder[A] =
    ZPure.get[State].flatMap {
      case State.Normal(component)     =>
        val firstFakeId = 1000000
        ZPure
          .set(State.Capturing(component, firstFakeId, firstFakeId, Chunk.empty))
          .zipRight(inner)
          .zipLeft(finishAddingManyComponentTypes)
      case State.Capturing(_, _, _, _) =>
        ZPure.fail("Cannot nest addManyComponentTypes")
    }

  private def finishAddingManyComponentTypes: ComponentBuilder[Unit] =
    ZPure.get[State].flatMap {
      case State.Capturing(component, firstFakeId, _, newSections) if newSections.nonEmpty =>
        val indexedNewComponentTypes = newSections.toMap
        Dependencies.topologicalSort(indexedNewComponentTypes) match {
          case Some(order) =>
            println(s"Topological sort: $order")
            val mapping = createMappingFromOrder(component, order)

            ZPure.set(State.Normal(component)) *>
              ZPure.foreachDiscard(order) { fakeId =>
                val componentType = indexedNewComponentTypes(fakeId)
                (componentType match {
                  case componentType: ComponentType     =>
                    addComponentType(componentType, InsertionPoint.End)
                  case componentImport: ComponentImport =>
                    addComponentImport(componentImport, InsertionPoint.End)
                  case alias: Alias                     =>
                    addAlias(alias, InsertionPoint.End)
                }).map { r =>
                  println(s"Added item ${componentType.getClass.getSimpleName} $fakeId with final id $r")
                  r
                }
              } *> mapAllSectionReference(mapping)

          case None =>
            ZPure.fail("Could not get a topological sort for the new component types")
        }
      case _                                                                               =>
        ZPure.unit
    }

  private def createMappingFromOrder(component: Component, order: Chunk[SectionReference]): SectionReference.Mapper =
    new SectionReference.Mapper {
      private lazy val fakeComponentTypeIdToRealId =
        order
          .collect { case SectionReference.ComponentType(typeIdx) => typeIdx }
          .zipWithIndexFrom(component.lastComponentTypeIdx.next.toInt)
          .toMap
          .map((ct, i) => (ct, ComponentTypeIdx.fromInt(i)))

      private lazy val fakeInstanceIdToRealId =
        order
          .collect { case SectionReference.Instance(instanceIdx) => instanceIdx }
          .zipWithIndexFrom(component.lastInstanceIdx.next.toInt)
          .toMap
          .map((ct, i) => (ct, InstanceIdx.fromInt(i)))

      println("fakeComponentTypeIdToRealId: " + fakeComponentTypeIdToRealId)
      println("fakeInstanceIdToRealId: " + fakeInstanceIdToRealId)

      override def map[S <: SectionReference: ClassTag](value: S): S =
        value match {
          case SectionReference.ComponentType(typeIdx) =>
            SectionReference.ComponentType(fakeComponentTypeIdToRealId.getOrElse(typeIdx, typeIdx)).asInstanceOf[S]
          case SectionReference.Instance(instanceIdx)  =>
            SectionReference.Instance(fakeInstanceIdToRealId.getOrElse(instanceIdx, instanceIdx)).asInstanceOf[S]
          case other: S                                =>
            other // TODO support all
        }
    }

  def getComponent(componentIdx: ComponentIdx): ComponentBuilder[Component] =
    ZPure.get[State].map(_.component.getComponent(componentIdx)).flatMap {
      case Some(component) => ZPure.succeed(component)
      case None            => ZPure.fail(s"Component $componentIdx not found")
    }

  def modifyAllComponentTypes(f: ComponentType => ComponentType): ComponentBuilder[Unit] =
    ZPure.get[State].flatMap {
      case State.Normal(_)                                                  =>
        ZPure.update(_.mapComponent(_.mapAllComponentTypes(f)))
      case State.Capturing(component, firstFakeId, nextFakeId, newSections) =>
        ZPure.set(
          State.Capturing(
            component.mapAllComponentTypes(f),
            firstFakeId,
            nextFakeId,
            newSections.map {
              case (idx, componentType: ComponentType) => (idx, f(componentType))
              case other                               => other
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

  def mapAllComponenentTypeIdx(f: ComponentTypeIdx => ComponentTypeIdx): ComponentBuilder[Unit] =
    ZPure.update(_.mapComponent(_.mapAllComponenentTypeIdx(f)))

  def mapAllSectionReference(f: SectionReference.Mapper): ComponentBuilder[Unit] =
    ZPure.get[State].flatMap {
      case State.Normal(_)                                                  =>
        ZPure.update(_.mapComponent(_.mapAllSectionReference(f)))
      case State.Capturing(component, firstFakeId, nextFakeId, newSections) =>
        ZPure.set(
          State.Capturing(
            component.mapAllSectionReference(f),
            firstFakeId,
            nextFakeId,
            newSections.map { case (idx, section) => (idx -> component.mapSectionReference(section, f)) }
          )
        )
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
    case Capturing(
        component: Component,
        firstFakeId: Int,
        nextFakeId: Int,
        newSections: Chunk[(SectionReference, Section[ComponentIndexSpace])]
    )

    def component: Component

    def mapComponent(f: Component => Component): State =
      this match {
        case Normal(component)                                          =>
          Normal(f(component))
        case Capturing(component, firstFakeId, nextFakeId, newSections) =>
          Capturing(f(component), firstFakeId, nextFakeId, newSections)
      }

    def modifyComponent[A](f: Component => (A, Component)): (A, State) = {
      val (result, newComponent) = f(component)
      (
        result,
        this match {
          case Normal(component)                                          =>
            Normal(newComponent)
          case Capturing(component, firstFakeId, nextFakeId, newSections) =>
            Capturing(newComponent, firstFakeId, nextFakeId, newSections)
        }
      )
    }
  }

  object State {
    def apply(component: Component): State = State.Normal(component)
  }
}
