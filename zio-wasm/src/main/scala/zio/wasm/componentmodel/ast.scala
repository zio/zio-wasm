package zio.wasm.componentmodel

import zio.*
import zio.wasm.*

import java.nio.charset.StandardCharsets

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

  lazy val coreInstanceIndex  = sections.indexed(ComponentIndexSpace.CoreInstance)
  lazy val instanceIndex      = sections.indexed(ComponentIndexSpace.Instance)
  lazy val componentTypeIndex = sections.indexed(ComponentIndexSpace.Type)
  lazy val coreFuncIndex      = sections.indexed(ComponentIndexSpace.CoreFunc)
  lazy val funcIndex          = sections.indexed(ComponentIndexSpace.Func)
  lazy val componentIndex     = sections.indexed(ComponentIndexSpace.Component)
  lazy val valueIndex         = sections.indexed(ComponentIndexSpace.Value)
  lazy val exportIndex        = sections.indexed(ComponentIndexSpace.Export)

  lazy val lastComponentIdx: ComponentIdx         = ComponentIdx.fromInt(componentIndex.size - 1)
  lazy val lastComponentTypeIdx: ComponentTypeIdx = ComponentTypeIdx.fromInt(componentTypeIndex.size - 1)
  lazy val lastValueIdx: ValueIdx                 = ValueIdx.fromInt(valueIndex.size - 1)
  lazy val lastExportIdx: ExportIdx               = ExportIdx.fromInt(exportIndex.size - 1)

  def addComponent(component: Component): (ComponentIdx, Component) =
    (
      lastComponentIdx.next,
      this.copy(sections = sections.addToEnd(component)),
    )

  def addComponentImport(componentImport: ComponentImport): (ComponentTypeIdx | ValueIdx, Component) = {
    val updatedComponent                 = this.copy(sections = sections.addToLastGroup(componentImport))
    val idx: ComponentTypeIdx | ValueIdx = componentImport.desc match {
      case ExternDesc.Val(_) =>
        updatedComponent.lastValueIdx
      case _                 =>
        updatedComponent.lastComponentTypeIdx
    }
    (idx, updatedComponent)
  }

  def addComponentExport(componentExport: ComponentExport): (ExportIdx, Component) =
    (
      lastExportIdx.next,
      this.copy(sections = sections.addToLastGroup(componentExport)),
    )

  def idxOf(componentType: ComponentType): Option[ComponentTypeIdx] =
    componentTypeIndex.toList.collectFirst { case (idx, section) if section == componentType => idx }

  def idxOf(componentType: ComponentImport): Option[ComponentTypeIdx] =
    componentTypeIndex.toList.collectFirst { case (idx, section) if section == componentType => idx }

  def idxOf(componentType: ComponentExport): Option[ComponentTypeIdx] =
    componentTypeIndex.toList.collectFirst { case (idx, section) if section == componentType => idx }

  def idxOf(componentType: Alias): Option[ComponentTypeIdx] =
    componentTypeIndex.toList.collectFirst { case (idx, section) if section == componentType => idx }

  /** WARNING: This reindexes all the existing component types */
  def insertComponentTypeToBeginning(
      componentType: ComponentType | ComponentImport | ComponentExport | Alias
  ): (ComponentTypeIdx, Component) =
    componentType match {
      case ct: ComponentType   =>
        val updated = this.copy(sections = sections.addToFirstGroupStart(ct))
        (updated.idxOf(ct).get, updated)
      case ci: ComponentImport =>
        val updated = this.copy(sections = sections.addToFirstGroupStart(ci))
        (updated.idxOf(ci).get, updated)
      case ce: ComponentExport =>
        val updated = this.copy(sections = sections.addToFirstGroupStart(ce))
        (updated.idxOf(ce).get, updated)
      case a: Alias            =>
        val updated = this.copy(sections = sections.addToFirstGroupStart(a))
        (updated.idxOf(a).get, updated)
    }

  def addComponentType(
      componentType: ComponentType | ComponentImport | ComponentExport | Alias
  ): (ComponentTypeIdx, Component) =
    (
      lastComponentTypeIdx.next,
      this.copy(sections = componentType match {
        case ct: ComponentType   => sections.addToLastGroup(ct)
        case ci: ComponentImport => sections.addToLastGroup(ci)
        case ce: ComponentExport => sections.addToLastGroup(ce)
        case a: Alias            => sections.addToLastGroup(a)
      }),
    )

  def getComponent(idx: ComponentIdx): Option[Component] =
    componentIndex.get(idx).asInstanceOf[Option[Component]]

  def getCoreInstance(idx: CoreInstanceIdx): Option[Instance]  =
    coreInstanceIndex.get(idx).asInstanceOf[Option[Instance]]
  def getInstance(idx: InstanceIdx): Option[ComponentInstance] =
    instanceIndex.get(idx).asInstanceOf[Option[ComponentInstance]]

  def getComponentType(idx: ComponentTypeIdx): Option[ComponentType | ComponentImport | Alias] =
    componentTypeIndex.get(idx).asInstanceOf[Option[ComponentType | ComponentImport | Alias]]

  def mapAllComponentTypes(f: ComponentType => ComponentType): Component =
    this.copy(
      sections.mapSectionBySectionType(ComponentSectionType.ComponentTypeSection)(f)
    )

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
}

final case class ComponentInstantiationArg(
    name: Name,
    desc: ExternDesc
)

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
}

final case class AliasTarget(ct: Int, idx: Int)

enum Canon extends Section[ComponentIndexSpace] {
  case Lift(funcIdx: FuncIdx, opts: Chunk[CanonicalOption], functionType: ComponentTypeIdx)
  case Lower(funcIdx: ComponentFuncIdx, opts: Chunk[CanonicalOption])
  case ResourceNew(typeIdx: ComponentTypeIdx)
  case ResourceDrop(typ: ComponentValType)
  case ResourceRep(typeIdx: ComponentTypeIdx)

  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentCanonSection
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
}

final case class ExternName(name: Name, url: Url)

final case class ComponentExport(
    name: ExternName,
    kind: ComponentExternalKind,
    idx: Int,
    desc: Option[ExternDesc]
) extends Section[ComponentIndexSpace] { // TODO: index type
  override def sectionType: SectionType[ComponentIndexSpace] = ComponentSectionType.ComponentExportSection
}

enum ExternDesc {
  case Module(typeIdx: ComponentTypeIdx)
  case Func(typeIdx: ComponentTypeIdx)
  case Val(valType: ComponentValType)
  case Type(typeBounds: TypeBound)
  case Instance(typeIdx: ComponentTypeIdx)
  case Component(typeIdx: ComponentTypeIdx)
}

enum TypeBound {
  case Eq(typeIdx: ComponentTypeIdx)
  case SubResource
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
  case Defined(typIdx: TypeIdx)
}

enum ComponentType extends Section[ComponentIndexSpace] {
  case Defined(componentDefinedType: ComponentDefinedType)
  case Func(componentFuncType: ComponentFuncType)
  case Component(componentTypes: Chunk[ComponentTypeDeclaration])
  case Instance(instanceTypes: Chunk[InstanceTypeDeclaration])
  case Resource(representation: ValType, destructor: Option[FuncIdx])

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
}

final case class ComponentFuncType(params: Chunk[(Name, ComponentValType)], result: ComponentFuncResult)

enum ComponentFuncResult {
  case Unnamed(typ: ComponentValType)
  case Named(types: Chunk[(Name, ComponentValType)])
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
}

final case class VariantCase(
    name: Name,
    typ: Option[ComponentValType],
    refines: Option[Int]
) // TODO: LabelIdx? or a new?

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
    def next: ComponentTypeIdx = idx + 1
    def toInt: Int             = idx
  }
}

type ComponentFuncIdx = ComponentFuncIdx.ComponentFuncIdx
object ComponentFuncIdx {
  opaque type ComponentFuncIdx = Int
  def fromInt(value: Int): ComponentFuncIdx = value

  extension (idx: ComponentFuncIdx) {
    def next: ComponentFuncIdx = idx + 1
    def toInt: Int             = idx
  }
}

type ModuleIdx = ModuleIdx.ModuleIdx
object ModuleIdx {
  opaque type ModuleIdx = Int
  def fromInt(value: Int): ModuleIdx = value

  extension (idx: ModuleIdx) {
    def next: ModuleIdx = idx + 1
    def toInt: Int      = idx
  }
}

type ComponentIdx = ComponentIdx.ComponentIdx
object ComponentIdx {
  opaque type ComponentIdx = Int
  def fromInt(value: Int): ComponentIdx = value

  extension (idx: ComponentIdx) {
    def next: ComponentIdx = idx + 1
    def toInt: Int         = idx
  }
}

type CoreInstanceIdx = CoreInstanceIdx.CoreInstanceIdx
object CoreInstanceIdx {
  opaque type CoreInstanceIdx = Int
  def fromInt(value: Int): CoreInstanceIdx = value

  extension (idx: CoreInstanceIdx) {
    def next: CoreInstanceIdx = idx + 1
    def toInt: Int            = idx
  }
}

type InstanceIdx = InstanceIdx.InstanceIdx
object InstanceIdx {
  opaque type InstanceIdx = Int
  def fromInt(value: Int): InstanceIdx = value

  extension (idx: InstanceIdx) {
    def next: InstanceIdx = idx + 1
    def toInt: Int        = idx
  }
}

type ValueIdx = ValueIdx.ValueIdx
object ValueIdx {
  opaque type ValueIdx = Int
  def fromInt(value: Int): ValueIdx = value

  extension (idx: ValueIdx) {
    def next: ValueIdx = idx + 1
    def toInt: Int     = idx
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
