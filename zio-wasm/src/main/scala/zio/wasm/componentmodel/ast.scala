package zio.wasm.componentmodel

import zio.*
import zio.wasm.*

import java.nio.charset.StandardCharsets

// Based on https://github.com/WebAssembly/component-model/blob/main/design/mvp/Binary.md
// and https://github.com/bytecodealliance/wasm-tools/blob/main/crates/wasmparser/src/readers/component/

// TODO: we need to preserve the original section order and grouping! Alias and Type are sharing the index space (maybe more?)
final case class Component(
    modules: Chunk[Module],
    instances: Chunk[Instance],
    coreTypes: Chunk[CoreType],
    components: Chunk[Component],
    componentInstances: Chunk[ComponentInstance],
    aliases: Chunk[Alias],
    types: Chunk[ComponentType],
    canons: Chunk[Canon],
    start: Option[ComponentStart],
    imports: Chunk[ComponentImport],
    exports: Chunk[ComponentExport],
    custom: Chunk[Custom]
)

enum Instance {
  case Instantiate(moduleIdx: ModuleIdx, args: Chunk[InstantiationArg])
  case FromExports(exports: Chunk[Export])
}

enum InstantiationArgRef {
  case Instance(idx: InstanceIdx)
}

final case class InstantiationArg(
    name: Name,
    ref: InstantiationArgRef
)

enum ComponentInstance {
  case Instantiate(componentIdx: ComponentIdx, args: Chunk[ComponentInstantiationArg])
  case FromExports(exports: Chunk[ComponentExport])
}

final case class ComponentInstantiationArg(
    name: Name,
    kind: ComponentExternalKind,
    idx: Int
) // TODO: merge ComponentExternalKind with Idx

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

enum Alias {
  case InstanceExport(kind: ComponentExternalKind, instanceIdx: InstanceIdx, name: Name)
  case CoreInstanceExport(kind: ExportKind, instanceIdx: InstanceIdx, name: Name)
  case Outer(kind: OuterAliasKind, target: AliasTarget)
}

final case class AliasTarget(ct: Int, idx: Int)

enum Canon {
  case Lift(funcIdx: FuncIdx, opts: Chunk[CanonicalOption], functionType: ComponentTypeIdx)
  case Lower(funcIdx: ComponentFuncIdx, opts: Chunk[CanonicalOption])
  case ResourceNew(typeIdx: ComponentTypeIdx)
  case ResourceDrop(typ: ComponentValType)
  case ResourceRep(typeIdx: ComponentTypeIdx)
}

enum CanonicalOption {
  case Utf8
  case Utf16
  case CompactUtf16
  case Memory(idx: MemIdx)
  case Realloc(idx: FuncIdx)
  case PostReturn(idx: FuncIdx)
}

final case class ComponentStart()

final case class ComponentImport(name: ExternName, desc: ExternDesc)

final case class ExternName(name: Name, url: Url)

final case class ComponentExport(
    name: ExternName,
    kind: ComponentExternalKind,
    idx: Int,
    desc: Option[ExternDesc]
) // TODO: index type

// TODO: name? relation to ExportDesc? ComponentTypeRef?
enum ExternDesc {
  case Module(moduleIdx: ModuleIdx)
  case Func(funcIdx: ComponentFuncIdx)
  case Val(valType: ComponentValType)
  case Type(typeBounds: TypeBounds)
  case Instance(instanceIdx: InstanceIdx)
  case Component(componentIdx: ComponentIdx)
}

enum TypeBounds {
  case Eq(typeIdx: ComponentTypeIdx)
  case SubResource
}

enum CoreType {
  case Function(funcType: FuncType)
  case Module(moduleTypes: Chunk[ModuleDeclaration])
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

enum ComponentType {
  case Defined(componentDefinedType: ComponentDefinedType)
  case Func(componentFuncType: ComponentFuncType)
  case Component(componentTypes: Chunk[ComponentTypeDeclaration])
  case Instance(instanceTypes: Chunk[InstanceTypeDeclaration])
  case Resource(representation: ValType, destructor: Option[FuncIdx])
}

final case class ComponentFuncType(params: Chunk[(Name, ComponentValType)], result: ComponentFuncResult)

enum ComponentFuncResult {
  case Unnamed(typ: ComponentValType)
  case Named(types: Chunk[(Name, ComponentValType)])
}

// TODO: instance type declaration and component type declaration could be union types to share common cases
enum InstanceTypeDeclaration {
  case Core(typ: CoreType)
  case Type(typ: ComponentType)
  case Alias(alias: zio.wasm.componentmodel.Alias)
  case Export(name: ExternName, desc: ExternDesc)
}

enum ComponentTypeDeclaration {
  case Core(typ: CoreType)
  case Type(typ: ComponentType)
  case Alias(alias: zio.wasm.componentmodel.Alias)
  case Import(imp: ComponentImport)
  case Export(name: ExternName, desc: ExternDesc)
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

opaque type Id = String
object Id {
  def fromString(id: String): Id        = id
  def fromBytes(bytes: Chunk[Byte]): Id = new String(bytes.toArray, StandardCharsets.UTF_8)
}

extension (name: Id) {
  def toBytes: Chunk[Byte] = Chunk.fromArray(name.getBytes(StandardCharsets.UTF_8))
}

// TODO: Is this the same as ComponentFuncIdx??
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

type InstanceIdx = InstanceIdx.InstanceIdx
object InstanceIdx {
  opaque type InstanceIdx = Int
  def fromInt(value: Int): InstanceIdx = value

  extension (idx: InstanceIdx) {
    def next: InstanceIdx = idx + 1
    def toInt: Int        = idx
  }
}
