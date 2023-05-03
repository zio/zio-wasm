package zio.wasm.componentmodel

import zio.*
import zio.wasm.*
import zio.wasm.componentmodel.InsertionPoint.{End, LastOfGroup}

import java.nio.charset.StandardCharsets
import scala.reflect.ClassTag

// Based on https://github.com/WebAssembly/component-model/blob/main/design/mvp/Binary.md
// and https://github.com/bytecodealliance/wasm-tools/blob/main/crates/wasmparser/src/readers/component/

final case class Component(sections: Sections[ComponentIndexSpace]) extends Section[ComponentIndexSpace] {
  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentSection

  lazy val imports: Chunk[ComponentImport]      =
    sections.filterBySectionType(ComponentSectionType.ComponentImportSection)
  lazy val exports: Chunk[ComponentExport]      =
    sections.filterBySectionType(ComponentSectionType.ComponentExportSection)
  lazy val coreInstances: Chunk[Instance]       =
    sections.filterBySectionType(ComponentSectionType.ComponentCoreInstanceSection)
  lazy val instances: Chunk[ComponentInstance]  =
    sections.filterBySectionType(ComponentSectionType.ComponentInstanceSection)
  lazy val componentTypes: Chunk[ComponentType] =
    sections.filterBySectionType(ComponentSectionType.ComponentTypeSection)
  lazy val coreTypes: Chunk[CoreType]           =
    sections.filterBySectionType(ComponentSectionType.ComponentCoreTypeSection)
  lazy val canons: Chunk[Canon]                 =
    sections.filterBySectionType(ComponentSectionType.ComponentCanonSection)
  lazy val aliases: Chunk[Alias]                =
    sections.filterBySectionType(ComponentSectionType.ComponentAliasSection)

  lazy val coreInstanceIndex  = sections.indexed(ComponentIndexSpace.CoreInstance)
  lazy val instanceIndex      = sections.indexed(ComponentIndexSpace.Instance)
  lazy val componentTypeIndex = sections.indexed(ComponentIndexSpace.Type)
  lazy val coreFuncIndex      = sections.indexed(ComponentIndexSpace.CoreFunc)
  lazy val funcIndex          = sections.indexed(ComponentIndexSpace.Func)
  lazy val componentIndex     = sections.indexed(ComponentIndexSpace.Component)
  lazy val valueIndex         = sections.indexed(ComponentIndexSpace.Value)
  lazy val moduleIndex        = sections.indexed(ComponentIndexSpace.Module)

  lazy val lastComponentIdx: ComponentIdx         = ComponentIdx.fromInt(componentIndex.size - 1)
  lazy val lastComponentFuncIdx: ComponentFuncIdx = ComponentFuncIdx.fromInt(funcIndex.size - 1)
  lazy val lastComponentTypeIdx: ComponentTypeIdx = ComponentTypeIdx.fromInt(componentTypeIndex.size - 1)
  lazy val lastInstanceIdx: InstanceIdx           = InstanceIdx.fromInt(instanceIndex.size - 1)
  lazy val lastModuleIdx: ModuleIdx               = ModuleIdx.fromInt(instanceIndex.size - 1)
  lazy val lastValueIdx: ValueIdx                 = ValueIdx.fromInt(valueIndex.size - 1)

  /** Collects all Alias.Outer aliases from top level and from type definitions which are pointing outside of the
    * component, and makes them relative to the compoennt's root (so ct=1 is the parent component for each)
    */
  lazy val allOuterAliasesRelativeToRoot: Chunk[Alias.Outer] =
    aliases
      .collect { case outer: Alias.Outer =>
        outer
      }
      .concat(
        componentTypes
          .flatMap(_.collectAliases().collect[Alias.Outer] { case (level, Alias.Outer(kind, AliasTarget(ct, i))) =>
            Alias.Outer(kind, AliasTarget(ct - level, i))
          })
          .filter(_.target.ct >= 0)
      )

  def addComponent(
      component: Component,
      insertionPoint: InsertionPoint = InsertionPoint.End
  ): (ComponentIdx, Component) =
    (
      lastComponentIdx.next,
      this.copy(sections = insertionPoint match {
        case InsertionPoint.LastOfGroup => sections.addToLastGroup(component)
        case InsertionPoint.End         => sections.addToEnd(component)
      }),
    )

  def addComponentImport(
      componentImport: ComponentImport,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): (SectionReference, Component) = {
    val updatedComponent =
      insertionPoint match {
        case InsertionPoint.LastOfGroup =>
          this.copy(sections = sections.addToLastGroup(componentImport))
        case InsertionPoint.End         =>
          this.copy(sections = sections.addToEnd(componentImport))
      }

    val idx =
      componentImport.desc match {
        case ExternDesc.Val(_)       =>
          SectionReference.Value(updatedComponent.lastValueIdx.next)
        case ExternDesc.Type(_)      =>
          SectionReference.ComponentType(updatedComponent.lastComponentTypeIdx.next)
        case ExternDesc.Func(_)      =>
          SectionReference.ComponentFunc(updatedComponent.lastComponentFuncIdx.next)
        case ExternDesc.Module(_)    =>
          SectionReference.Module(updatedComponent.lastModuleIdx.next)
        case ExternDesc.Instance(_)  =>
          SectionReference.Instance(updatedComponent.lastInstanceIdx.next)
        case ExternDesc.Component(_) =>
          SectionReference.Component(updatedComponent.lastComponentIdx.next)
      }
    (idx, updatedComponent)
  }

  def addComponentInstance(
      componentInstance: ComponentInstance,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): (InstanceIdx, Component) =
    (
      lastInstanceIdx.next,
      insertionPoint match {
        case InsertionPoint.LastOfGroup => this.copy(sections = sections.addToLastGroup(componentInstance))
        case InsertionPoint.End         => this.copy(sections = sections.addToEnd(componentInstance))
      }
    )

  def addComponentExport(
      componentExport: ComponentExport,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): (SectionReference, Component) =
    (
      componentExport.kind match {
        case ComponentExternalKind.Module    => SectionReference.Module(lastModuleIdx.next)
        case ComponentExternalKind.Func      => SectionReference.ComponentFunc(lastComponentFuncIdx.next)
        case ComponentExternalKind.Value     => SectionReference.Value(lastValueIdx.next)
        case ComponentExternalKind.Type      => SectionReference.ComponentType(lastComponentTypeIdx.next)
        case ComponentExternalKind.Instance  => SectionReference.Instance(lastInstanceIdx.next)
        case ComponentExternalKind.Component => SectionReference.Component(lastComponentIdx.next)
      },
      insertionPoint match {
        case InsertionPoint.LastOfGroup => this.copy(sections = sections.addToLastGroup(componentExport))
        case InsertionPoint.End         => this.copy(sections = sections.addToEnd(componentExport))
      }
    )

  def addComponentType(
      componentType: ComponentType,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): (ComponentTypeIdx, Component) =
    (
      lastComponentTypeIdx.next,
      insertionPoint match {
        case InsertionPoint.LastOfGroup => this.copy(sections = sections.addToLastGroup(componentType))
        case InsertionPoint.End         => this.copy(sections = sections.addToEnd(componentType))
      }
    )

  def addAlias(
      alias: Alias,
      insertionPoint: InsertionPoint = InsertionPoint.LastOfGroup
  ): (SectionReference, Component) =
    (
      ComponentSectionType.ComponentAliasSection.indexSpace(alias) match {
        case ComponentIndexSpace.Component  => SectionReference.Component(lastComponentIdx.next)
        case ComponentIndexSpace.Instance   => SectionReference.Instance(lastInstanceIdx.next)
        case ComponentIndexSpace.Module     => SectionReference.Module(lastModuleIdx.next)
        case ComponentIndexSpace.Type       => SectionReference.ComponentType(lastComponentTypeIdx.next)
        case ComponentIndexSpace.Func       => SectionReference.ComponentFunc(lastComponentFuncIdx.next)
        case ComponentIndexSpace.Value      => SectionReference.Value(lastValueIdx.next)
        case ComponentIndexSpace.CoreType   => ??? // TODO
        case ComponentIndexSpace.CoreTable  => ??? // TODO
        case ComponentIndexSpace.CoreFunc   => ??? // TODO
        case ComponentIndexSpace.CoreGlobal => ??? // TODO
        case ComponentIndexSpace.CoreMem    => ??? // TODO
      },
      insertionPoint match {
        case LastOfGroup => this.copy(sections = sections.addToLastGroup(alias))
        case End         => this.copy(sections = sections.addToEnd(alias))
      }
    )

  def getComponent(idx: ComponentIdx): Option[Component] =
    componentIndex.get(idx).asInstanceOf[Option[Component]]

  def getCoreInstance(idx: CoreInstanceIdx): Option[Instance] =
    coreInstanceIndex.get(idx).asInstanceOf[Option[Instance]]

  def getInstance(idx: InstanceIdx): Option[ComponentInstance] =
    instanceIndex.get(idx).asInstanceOf[Option[ComponentInstance]]

  def getComponentType(idx: ComponentTypeIdx): Option[ComponentType | ComponentImport | Alias] =
    componentTypeIndex.get(idx).asInstanceOf[Option[ComponentType | ComponentImport | Alias]]

  def mapAllAlias(f: Alias => Alias): Component =
    this.copy(
      sections.mapSectionBySectionType(ComponentSectionType.ComponentAliasSection)(f)
    )

  def mapAllComponentTypes(f: ComponentType => ComponentType): Component =
    this.copy(
      sections.mapSectionBySectionType(ComponentSectionType.ComponentTypeSection)(f)
    )

  def mapAllComponenentTypeIdx(f: ComponentTypeIdx => ComponentTypeIdx, level: Int = 1): Component =
    mapAllSectionReference(new SectionReference.Mapper {
      override def map[S <: SectionReference: ClassTag](value: S): S =
        value match {
          case SectionReference.ComponentType(idx) =>
            SectionReference.ComponentType(f(idx)).asInstanceOf[S]
          case other: S                            =>
            other
        }
    })

  def mapAllSectionReference(f: SectionReference.Mapper): Component =
    this.copy(
      sections.map(mapSectionReference(_, f))
    )

  // TODO: this does not belong to here
  def mapSectionReference(
      section: Section[ComponentIndexSpace],
      f: SectionReference.Mapper
  ): Section[ComponentIndexSpace] =
    section match {
      case ct: ComponentType                   =>
        ct.mapSectionReference(1, f)
      case alias: Alias                        =>
        alias.mapSectionReference(1, f)
      case canon: Canon                        =>
        canon.mapSectionReference(f)
      case ci: ComponentImport                 =>
        ci.mapSectionReference(f)
      case ce: ComponentExport                 =>
        ce.mapSectionReference(f)
      case cinst: ComponentInstance            =>
        cinst.mapSectionReference(f)
      case c: Component                        =>
        // TODO: we need to recursively do this for all inner components!
        c.mapAllAlias {
          case alias @ Alias.Outer(kind, target) =>
            if (target.ct == 1) alias.mapSectionReference(1, f)
            else alias
          case alias: Alias                      => alias
        }.mapAllComponentTypes { ct =>
          ct.mapAliases {
            case (level, alias @ Alias.Outer(kind, target)) =>
              if (target.ct == level) alias.mapSectionReference(level, f)
              else alias
            case (_, alias)                                 =>
              alias
          }
        }
      case other: Section[ComponentIndexSpace] =>
        other
    }

  def moveToEnd(idx: ComponentIdx): Component =
    getComponent(idx) match {
      case Some(section) => this.copy(sections = sections.moveToEnd(section))
      case None          => this
    }

  def replaceComponent(idx: ComponentIdx, newComponent: Component): Component =
    this.copy(sections = sections.replace(ComponentIndexSpace.Component)(idx, newComponent))

  def replaceComponentType(idx: ComponentTypeIdx, newComponentType: ComponentType): Component =
    this.copy(sections = sections.replace(ComponentIndexSpace.Type)(idx, newComponentType))

  def replaceComponentType(idx: ComponentTypeIdx, alias: Alias): Component =
    this.copy(sections = sections.replace(ComponentIndexSpace.Type)(idx, alias))

  lazy val sectionToSectionReference: Map[Section[ComponentIndexSpace], SectionReference] =
    (coreInstanceIndex.map { case (k, v) => (v, SectionReference.CoreInstance(k)) } ++
      instanceIndex.map { case (k, v) => (v, SectionReference.Instance(k)) } ++
      componentTypeIndex.map { case (k, v) => (v, SectionReference.ComponentType(k)) } ++
      funcIndex.map { case (k, v) => (v, SectionReference.ComponentFunc(k)) } ++
      componentIndex.map { case (k, v) => (v, SectionReference.Component(k)) } ++
      valueIndex.map { case (k, v) => (v, SectionReference.Value(k)) } ++
      moduleIndex.map { case (k, v) => (v, SectionReference.Module(k)) })

  def sectionReference(section: Section[ComponentIndexSpace]): SectionReference =
    sectionToSectionReference(section)
}

object Component {
  val empty: Component = Component(Sections(Chunk.empty))
}

enum Instance extends Section[ComponentIndexSpace] {
  case Instantiate(moduleIdx: ModuleIdx, args: Chunk[InstantiationArg])
  case FromExports(exports: Chunk[Export])

  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentCoreInstanceSection
}

enum InstantiationArgRef {
  case Instance(idx: InstanceIdx)
}

final case class InstantiationArg(
    name: Name,
    ref: InstantiationArgRef
)

enum ComponentInstance extends Section[ComponentIndexSpace] {
  case Instantiate(componentIdx: ComponentIdx, args: Chunk[ComponentInstantiationArg])
  case FromExports(exports: Chunk[ComponentExport])

  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentInstanceSection

  def mapSectionReference(f: SectionReference.Mapper): ComponentInstance =
    this match {
      case Instantiate(componentIdx, args) =>
        Instantiate(f.map(componentIdx.toSectionReference).idx, args.map(_.mapSectionReference(f)))
      case FromExports(exports)            =>
        FromExports(exports.map(_.mapSectionReference(f)))
    }
}

final case class ComponentInstantiationArg(
    name: Name,
    kind: ComponentExternalKind,
    idx: Int
) {

  def mapSectionReference(f: SectionReference.Mapper): ComponentInstantiationArg =
    kind match {
      case ComponentExternalKind.Module    =>
        this.copy(
          idx = f.map[SectionReference.Module](SectionReference.Module(ModuleIdx.fromInt(idx))).idx.toInt
        )
      case ComponentExternalKind.Func      =>
        this.copy(
          idx = f
            .map[SectionReference.ComponentFunc](SectionReference.ComponentFunc(ComponentFuncIdx.fromInt(idx)))
            .idx
            .toInt
        )
      case ComponentExternalKind.Value     =>
        this.copy(
          idx = f.map[SectionReference.Value](SectionReference.Value(ValueIdx.fromInt(idx))).idx.toInt
        )
      case ComponentExternalKind.Type      =>
        this.copy(
          idx = f
            .map[SectionReference.ComponentType](SectionReference.ComponentType(ComponentTypeIdx.fromInt(idx)))
            .idx
            .toInt
        )
      case ComponentExternalKind.Instance  =>
        this.copy(
          idx = f.map[SectionReference.Instance](SectionReference.Instance(InstanceIdx.fromInt(idx))).idx.toInt
        )
      case ComponentExternalKind.Component =>
        this.copy(
          idx = f.map[SectionReference.Component](SectionReference.Component(ComponentIdx.fromInt(idx))).idx.toInt
        )
    }
}

enum ComponentExternalKind {
  case Module
  case Func
  case Value
  case Type
  case Instance
  case Component
}

enum OuterAliasKind {
  case CoreModule
  case CoreType
  case Type
  case Component
}

enum ExportKind {
  case Func
  case Table
  case Mem
  case Global
}

enum Alias extends Section[ComponentIndexSpace] {
  case InstanceExport(kind: ComponentExternalKind, instanceIdx: InstanceIdx, name: Name)
  case CoreInstanceExport(kind: ExportKind, instanceIdx: InstanceIdx, name: Name)
  case Outer(kind: OuterAliasKind, target: AliasTarget)

  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentAliasSection

  def mapSectionReference(level: Int, f: SectionReference.Mapper): Alias =
    this match {
      case InstanceExport(kind, instanceIdx, name)     =>
        InstanceExport(kind, f.map(instanceIdx.toSectionReference).idx, name)
      case CoreInstanceExport(kind, instanceIdx, name) =>
        CoreInstanceExport(kind, f.map(instanceIdx.toSectionReference).idx, name)
      case Outer(kind, target)                         =>
        if (level == target.ct)
          kind match {
            case OuterAliasKind.Type      =>
              Outer(
                kind,
                target.copy(idx =
                  f.map[SectionReference.ComponentType](ComponentTypeIdx.fromInt(target.idx).toSectionReference)
                    .idx
                    .toInt
                )
              )
            case OuterAliasKind.Component =>
              Outer(
                kind,
                target.copy(idx =
                  f.map[SectionReference.Component](ComponentIdx.fromInt(target.idx).toSectionReference).idx.toInt
                )
              )
            case _                        =>
              // TODO
              this
          }
        else
          this
    }
}

final case class AliasTarget(ct: Int, idx: Int)

enum Canon extends Section[ComponentIndexSpace] {
  case Lift(funcIdx: FuncIdx, opts: Chunk[CanonicalOption], functionType: ComponentTypeIdx)
  case Lower(funcIdx: ComponentFuncIdx, opts: Chunk[CanonicalOption])
  case ResourceNew(typeIdx: ComponentTypeIdx)
  case ResourceDrop(typ: ComponentValType)
  case ResourceRep(typeIdx: ComponentTypeIdx)

  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentCanonSection

  def mapSectionReference(f: SectionReference.Mapper): Canon =
    this match {
      case Lift(funcIdx, opts, functionType) =>
        Lift(funcIdx, opts, f.map(functionType.toSectionReference).idx)
      case Lower(funcIdx, opts)              =>
        Lower(f.map(funcIdx.toSectionReference).idx, opts)
      case ResourceNew(typeIdx)              =>
        ResourceNew(f.map(typeIdx.toSectionReference).idx)
      case ResourceDrop(typ)                 =>
        ResourceDrop(typ.mapSectionReference(f))
      case ResourceRep(typeIdx)              =>
        ResourceRep(f.map(typeIdx.toSectionReference).idx)
    }
}

enum CanonicalOption {
  case Utf8
  case Utf16
  case CompactUtf16
  case Memory(idx: MemIdx)
  case Realloc(idx: FuncIdx)
  case PostReturn(idx: FuncIdx)
}

final case class ComponentStart(funcIdx: ComponentFuncIdx, args: Chunk[ValueIdx], results: Int)
    extends Section[ComponentIndexSpace] {
  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentStartSection
}

final case class ComponentImport(name: ExternName, desc: ExternDesc) extends Section[ComponentIndexSpace] {
  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentImportSection

  def mapSectionReference(f: SectionReference.Mapper): ComponentImport =
    this.copy(desc = desc.mapSectionReference(f))
}

final case class ExternName(name: Name, url: Url)

final case class ComponentExport(
    name: ExternName,
    kind: ComponentExternalKind,
    idx: Int,
    desc: Option[ExternDesc]
) extends Section[ComponentIndexSpace] { // TODO: index type
  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentExportSection

  def mapSectionReference(f: SectionReference.Mapper): ComponentExport =
    kind match {
      case ComponentExternalKind.Module    =>
        this.copy(
          idx = f.map[SectionReference.Module](SectionReference.Module(ModuleIdx.fromInt(idx))).idx.toInt,
          desc = desc.map(_.mapSectionReference(f))
        )
      case ComponentExternalKind.Func      =>
        this.copy(
          idx = f
            .map[SectionReference.ComponentFunc](SectionReference.ComponentFunc(ComponentFuncIdx.fromInt(idx)))
            .idx
            .toInt,
          desc = desc.map(_.mapSectionReference(f))
        )
      case ComponentExternalKind.Value     =>
        this.copy(
          idx = f.map[SectionReference.Value](SectionReference.Value(ValueIdx.fromInt(idx))).idx.toInt,
          desc = desc.map(_.mapSectionReference(f))
        )
      case ComponentExternalKind.Type      =>
        this.copy(
          idx = f
            .map[SectionReference.ComponentType](SectionReference.ComponentType(ComponentTypeIdx.fromInt(idx)))
            .idx
            .toInt,
          desc = desc.map(_.mapSectionReference(f))
        )
      case ComponentExternalKind.Instance  =>
        this.copy(
          idx = f.map[SectionReference.Instance](SectionReference.Instance(InstanceIdx.fromInt(idx))).idx.toInt,
          desc = desc.map(_.mapSectionReference(f))
        )
      case ComponentExternalKind.Component =>
        this.copy(
          idx = f.map[SectionReference.Component](SectionReference.Component(ComponentIdx.fromInt(idx))).idx.toInt,
          desc = desc.map(_.mapSectionReference(f))
        )
    }
}

enum ExternDesc {
  case Module(typeIdx: ComponentTypeIdx)
  case Func(typeIdx: ComponentTypeIdx)
  case Val(valType: ComponentValType)
  case Type(typeBounds: TypeBound)
  case Instance(typeIdx: ComponentTypeIdx)
  case Component(typeIdx: ComponentTypeIdx)

  def mapSectionReference(f: SectionReference.Mapper): ExternDesc =
    this match {
      case ExternDesc.Module(idx)      => ExternDesc.Module(f.map(idx.toSectionReference).idx)
      case ExternDesc.Func(idx)        => ExternDesc.Func(f.map(idx.toSectionReference).idx)
      case ExternDesc.Val(valType)     => ExternDesc.Val(valType.mapSectionReference(f))
      case ExternDesc.Type(typeBounds) => ExternDesc.Type(typeBounds.mapSectionReference(f))
      case ExternDesc.Instance(idx)    => ExternDesc.Instance(f.map(idx.toSectionReference).idx)
      case ExternDesc.Component(idx)   => ExternDesc.Component(f.map(idx.toSectionReference).idx)
    }
}

enum TypeBound {
  case Eq(typeIdx: ComponentTypeIdx)
  case SubResource

  def mapSectionReference(f: SectionReference.Mapper): TypeBound =
    this match {
      case TypeBound.Eq(idx)     => TypeBound.Eq(f.map(idx.toSectionReference).idx)
      case TypeBound.SubResource => this
    }
}

enum CoreType extends Section[ComponentIndexSpace] {
  case Function(funcType: FuncType)
  case Module(moduleTypes: Chunk[ModuleDeclaration])

  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentCoreTypeSection
}

enum ModuleDeclaration {
  case Type(typ: zio.wasm.Type)
  case Export(name: Name, desc: ExportDesc)
  case OuterAlias(alias: Alias.Outer)
  case Import(imp: zio.wasm.Import)
}

enum ComponentValType {
  case Primitive(typ: PrimitiveValueType)
  case Defined(typIdx: ComponentTypeIdx)

  def mapSectionReference(f: SectionReference.Mapper): ComponentValType =
    this match {
      case ComponentValType.Primitive(_) => this
      case ComponentValType.Defined(idx) => ComponentValType.Defined(f.map(idx.toSectionReference).idx)
    }
}

enum ComponentType extends Section[ComponentIndexSpace] {
  case Defined(componentDefinedType: ComponentDefinedType)
  case Func(componentFuncType: ComponentFuncType)
  case Component(componentTypes: Chunk[ComponentTypeDeclaration])
  case Instance(instanceTypes: Chunk[InstanceTypeDeclaration])
  case Resource(representation: ValType, destructor: Option[ComponentFuncIdx])

  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentTypeSection

  /** Collect all the aliases in this type tree.
    *
    * For each Alias it also returns the _depth_ of the alias in the tree, so the Alias.Outer can be interpreted
    * properly. The top level component/instance type's alias members will have depth 1, so an outer alias ct=1 refers
    * to this component type's owner.
    */
  def collectAliases(atLevel: Int = 1): Chunk[(Int, Alias)] =
    this match {
      case ComponentType.Component(types) => types.flatMap(_.collectAliases(atLevel))
      case ComponentType.Instance(types)  => types.flatMap(_.collectAliases(atLevel))
      case _                              => Chunk.empty
    }

  def mapAliases(f: (Int, Alias) => Alias, atLevel: Int = 1): ComponentType =
    this match {
      case ComponentType.Component(types) =>
        ComponentType.Component(types.map(_.mapAliases(f, atLevel)))
      case ComponentType.Instance(types)  =>
        ComponentType.Instance(types.map(_.mapAliases(f, atLevel)))
      case _                              => this
    }

  def mapSectionReference(level: Int, f: SectionReference.Mapper): ComponentType =
    this match {
      case ComponentType.Defined(typ)                             => ComponentType.Defined(typ.mapSectionReference(f))
      case ComponentType.Func(typ)                                => ComponentType.Func(typ.mapSectionReference(f))
      case ComponentType.Resource(repr, destructor)               => ComponentType.Resource(repr, destructor)
      case ComponentType.Component(_) | ComponentType.Instance(_) =>
        // We cannot map the references within a component or instance declaration because they introduce
        // a new scope. We still need to replace outer aliases pointing to the current scope.
        mapAliases(
          {
            case (level, alias @ Alias.Outer(kind, target)) =>
              if (target.ct == level) alias.mapSectionReference(level, f)
              else alias
            case (_, alias)                                 =>
              alias
          },
          level
        )
    }
}

final case class ComponentFuncType(params: Chunk[(Name, ComponentValType)], result: ComponentFuncResult) {

  def mapSectionReference(f: SectionReference.Mapper): ComponentFuncType =
    ComponentFuncType(
      params.map { case (name, typ) => (name, typ.mapSectionReference(f)) },
      result.mapSectionReference(f)
    )
}

enum ComponentFuncResult {
  case Unnamed(typ: ComponentValType)
  case Named(types: Chunk[(Name, ComponentValType)])

  def mapSectionReference(f: SectionReference.Mapper): ComponentFuncResult =
    this match {
      case ComponentFuncResult.Unnamed(typ) =>
        ComponentFuncResult.Unnamed(typ.mapSectionReference(f))
      case ComponentFuncResult.Named(types) =>
        ComponentFuncResult.Named(types.map { case (name, typ) => (name, typ.mapSectionReference(f)) })
    }
}

enum InstanceTypeDeclaration {
  case Core(typ: CoreType)
  case Type(typ: ComponentType)
  case Alias(alias: zio.wasm.componentmodel.Alias)
  case Export(name: ExternName, desc: ExternDesc)

  def collectAliases(atLevel: Int): Chunk[(Int, zio.wasm.componentmodel.Alias)] =
    this match {
      case InstanceTypeDeclaration.Alias(alias) => Chunk((atLevel, alias))
      case InstanceTypeDeclaration.Type(typ)    => typ.collectAliases(atLevel + 1)
      case _                                    => Chunk.empty
    }

  def mapAliases(
      f: (Int, zio.wasm.componentmodel.Alias) => zio.wasm.componentmodel.Alias,
      atLevel: Int
  ): InstanceTypeDeclaration =
    this match {
      case InstanceTypeDeclaration.Alias(alias) => InstanceTypeDeclaration.Alias(f(atLevel, alias))
      case InstanceTypeDeclaration.Type(typ)    => InstanceTypeDeclaration.Type(typ.mapAliases(f, atLevel + 1))
      case _                                    => this
    }

  def mapSectionReference(level: Int, f: SectionReference.Mapper): InstanceTypeDeclaration =
    this match {
      case InstanceTypeDeclaration.Core(typ)          =>
        this
      case InstanceTypeDeclaration.Type(typ)          =>
        InstanceTypeDeclaration.Type(typ.mapSectionReference(level + 1, f))
      case InstanceTypeDeclaration.Alias(alias)       =>
        InstanceTypeDeclaration.Alias(alias.mapSectionReference(level, f))
      case InstanceTypeDeclaration.Export(name, desc) =>
        InstanceTypeDeclaration.Export(name, desc.mapSectionReference(f))
    }
}

enum ComponentTypeDeclaration {
  case Core(typ: CoreType)
  case Type(typ: ComponentType)
  case Alias(alias: zio.wasm.componentmodel.Alias)
  case Import(imp: ComponentImport)
  case Export(name: ExternName, desc: ExternDesc)

  def collectAliases(atLevel: Int): Chunk[(Int, zio.wasm.componentmodel.Alias)] =
    this match {
      case ComponentTypeDeclaration.Alias(alias) => Chunk((atLevel, alias))
      case ComponentTypeDeclaration.Type(typ)    => typ.collectAliases(atLevel + 1)
      case _                                     => Chunk.empty
    }

  def mapAliases(
      f: (Int, zio.wasm.componentmodel.Alias) => zio.wasm.componentmodel.Alias,
      atLevel: Int
  ): ComponentTypeDeclaration =
    this match {
      case ComponentTypeDeclaration.Alias(alias) => ComponentTypeDeclaration.Alias(f(atLevel, alias))
      case ComponentTypeDeclaration.Type(typ)    => ComponentTypeDeclaration.Type(typ.mapAliases(f, atLevel + 1))
      case _                                     => this
    }

  def mapSectionReference(level: Int, f: SectionReference.Mapper): ComponentTypeDeclaration =
    this match {
      case ComponentTypeDeclaration.Core(typ)          =>
        this
      case ComponentTypeDeclaration.Type(typ)          =>
        ComponentTypeDeclaration.Type(typ.mapSectionReference(level + 1, f))
      case ComponentTypeDeclaration.Alias(alias)       =>
        ComponentTypeDeclaration.Alias(alias.mapSectionReference(level, f))
      case ComponentTypeDeclaration.Import(imp)        =>
        ComponentTypeDeclaration.Import(imp.mapSectionReference(f))
      case ComponentTypeDeclaration.Export(name, desc) =>
        ComponentTypeDeclaration.Export(name, desc.mapSectionReference(f))
    }
}

enum ComponentDefinedType {
  case Primitive(typ: PrimitiveValueType)
  case Record(fields: Chunk[(Name, ComponentValType)])
  case Variant(cases: Chunk[VariantCase])
  case List(elem: ComponentValType)
  case Tuple(elems: Chunk[ComponentValType])
  case Flags(names: NonEmptyChunk[Name])
  case Enum(cases: NonEmptyChunk[Name])
  case Union(types: Chunk[ComponentValType])
  case Option(typ: ComponentValType)
  case Result(ok: scala.Option[ComponentValType], err: scala.Option[ComponentValType])
  case Own(typeIdx: ComponentTypeIdx)
  case Borrow(typeIdx: ComponentTypeIdx)

  def mapSectionReference(f: SectionReference.Mapper): ComponentDefinedType =
    this match {
      case ComponentDefinedType.Primitive(typ)  => this
      case ComponentDefinedType.Record(fields)  =>
        ComponentDefinedType.Record(fields.map { case (name, typ) => (name, typ.mapSectionReference(f)) })
      case ComponentDefinedType.Variant(cases)  =>
        ComponentDefinedType.Variant(cases.map(c => c.copy(typ = c.typ.map(_.mapSectionReference(f)))))
      case ComponentDefinedType.List(elem)      => ComponentDefinedType.List(elem.mapSectionReference(f))
      case ComponentDefinedType.Tuple(elems)    => ComponentDefinedType.Tuple(elems.map(_.mapSectionReference(f)))
      case ComponentDefinedType.Flags(names)    => this
      case ComponentDefinedType.Enum(cases)     => this
      case ComponentDefinedType.Union(types)    => ComponentDefinedType.Union(types.map(_.mapSectionReference(f)))
      case ComponentDefinedType.Option(typ)     => ComponentDefinedType.Option(typ.mapSectionReference(f))
      case ComponentDefinedType.Result(ok, err) =>
        ComponentDefinedType.Result(ok.map(_.mapSectionReference(f)), err.map(_.mapSectionReference(f)))
      case ComponentDefinedType.Own(typeIdx)    => this
      case ComponentDefinedType.Borrow(typeIdx) => this
    }
}

final case class VariantCase(
    name: Name,
    typ: Option[ComponentValType],
    refines: Option[Int] // TODO: LabelIdx? or a new?
) {

  def mapSectionReference(f: SectionReference.Mapper): VariantCase =
    this.copy(typ = typ.map(_.mapSectionReference(f)))
}

enum PrimitiveValueType {
  case Bool
  case S8
  case U8
  case S16
  case U16
  case S32
  case U32
  case S64
  case U64
  case F32
  case F64
  case Chr
  case Str
}

final case class Custom(name: Name, data: Chunk[Byte]) extends Section[ComponentIndexSpace] {
  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentCustomSection
}

opaque type Id = String
object Id {
  def fromString(id: String): Id        = id
  def fromBytes(bytes: Chunk[Byte]): Id = new String(bytes.toArray, StandardCharsets.UTF_8)
}

extension (name: Id) {
  def toBytes: Chunk[Byte] = Chunk.fromArray(name.getBytes(StandardCharsets.UTF_8))
}

type ComponentTypeIdx = ComponentTypeIdx.ComponentTypeIdx
object ComponentTypeIdx {
  opaque type ComponentTypeIdx = Int
  def fromInt(value: Int): ComponentTypeIdx = value

  extension (idx: ComponentTypeIdx) {
    def next: ComponentTypeIdx                             = idx + 1
    def toInt: Int                                         = idx
    def toSectionReference: SectionReference.ComponentType = SectionReference.ComponentType(idx)
  }
}

type ComponentFuncIdx = ComponentFuncIdx.ComponentFuncIdx
object ComponentFuncIdx {
  opaque type ComponentFuncIdx = Int
  def fromInt(value: Int): ComponentFuncIdx = value

  extension (idx: ComponentFuncIdx) {
    def next: ComponentFuncIdx                             = idx + 1
    def toInt: Int                                         = idx
    def toSectionReference: SectionReference.ComponentFunc = SectionReference.ComponentFunc(idx)
  }
}

type ModuleIdx = ModuleIdx.ModuleIdx
object ModuleIdx {
  opaque type ModuleIdx = Int
  def fromInt(value: Int): ModuleIdx = value

  extension (idx: ModuleIdx) {
    def next: ModuleIdx                             = idx + 1
    def toInt: Int                                  = idx
    def toSectionReference: SectionReference.Module = SectionReference.Module(idx)
  }
}

type ComponentIdx = ComponentIdx.ComponentIdx
object ComponentIdx {
  opaque type ComponentIdx = Int
  def fromInt(value: Int): ComponentIdx = value

  extension (idx: ComponentIdx) {
    def next: ComponentIdx                             = idx + 1
    def toInt: Int                                     = idx
    def toSectionReference: SectionReference.Component = SectionReference.Component(idx)
  }
}

type CoreInstanceIdx = CoreInstanceIdx.CoreInstanceIdx
object CoreInstanceIdx {
  opaque type CoreInstanceIdx = Int
  def fromInt(value: Int): CoreInstanceIdx = value

  extension (idx: CoreInstanceIdx) {
    def next: CoreInstanceIdx                             = idx + 1
    def toInt: Int                                        = idx
    def toSectionReference: SectionReference.CoreInstance = SectionReference.CoreInstance(idx)
  }
}

type InstanceIdx = InstanceIdx.InstanceIdx
object InstanceIdx {
  opaque type InstanceIdx = Int
  def fromInt(value: Int): InstanceIdx = value

  extension (idx: InstanceIdx) {
    def next: InstanceIdx                             = idx + 1
    def toInt: Int                                    = idx
    def toSectionReference: SectionReference.Instance = SectionReference.Instance(idx)
  }
}

type ValueIdx = ValueIdx.ValueIdx
object ValueIdx {
  opaque type ValueIdx = Int
  def fromInt(value: Int): ValueIdx = value

  extension (idx: ValueIdx) {
    def next: ValueIdx                             = idx + 1
    def toInt: Int                                 = idx
    def toSectionReference: SectionReference.Value = SectionReference.Value(idx)
  }
}

type StartIdx = StartIdx.StartIdx
object StartIdx {
  opaque type StartIdx = Int
  def fromInt(value: Int): StartIdx = value

  extension (idx: StartIdx) {
    def next: StartIdx = idx + 1
    def toInt: Int     = idx
  }
}

enum InsertionPoint {
  case LastOfGroup
  case End
}
