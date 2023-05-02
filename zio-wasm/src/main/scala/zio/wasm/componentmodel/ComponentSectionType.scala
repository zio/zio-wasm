package zio.wasm.componentmodel

import zio.wasm.{Module, SectionType}
import zio.wasm.syntax.Binary.{SectionId, labelIdx}

import scala.reflect.ClassTag

object ComponentSectionType {

  object ComponentModuleSection extends SectionType[ComponentIndexSpace] {
    override type Section = Module
    def ct: ClassTag[Section]                                    = implicitly[ClassTag[Module]]
    override def binaryId: SectionId                             = SectionId.fromByte(1)
    override def indexSpace(module: Module): ComponentIndexSpace = ComponentIndexSpace.Module
    override def allowsGrouping: Boolean                         = false
  }

  object ComponentCoreInstanceSection extends SectionType[ComponentIndexSpace] {
    override type Section = Instance
    def ct: ClassTag[Section]                                        = implicitly[ClassTag[Instance]]
    override def binaryId: SectionId                                 = SectionId.fromByte(2)
    override def indexSpace(instance: Instance): ComponentIndexSpace = ComponentIndexSpace.CoreInstance
    override def allowsGrouping: Boolean                             = true
  }

  object ComponentCoreTypeSection extends SectionType[ComponentIndexSpace] {
    override type Section = CoreType
    def ct: ClassTag[Section]                                        = implicitly[ClassTag[CoreType]]
    override def binaryId: SectionId                                 = SectionId.fromByte(3)
    override def indexSpace(coreType: CoreType): ComponentIndexSpace = ComponentIndexSpace.CoreType
    override def allowsGrouping: Boolean                             = true
  }

  object ComponentSection extends SectionType[ComponentIndexSpace] {
    override type Section = Component
    def ct: ClassTag[Section]                                          = implicitly[ClassTag[Component]]
    override def binaryId: SectionId                                   = SectionId.fromByte(4)
    override def indexSpace(component: Component): ComponentIndexSpace = ComponentIndexSpace.Component
    override def allowsGrouping: Boolean                               = false
  }

  object ComponentInstanceSection extends SectionType[ComponentIndexSpace] {
    override type Section = ComponentInstance
    def ct: ClassTag[Section]                                                          = implicitly[ClassTag[ComponentInstance]]
    override def binaryId: SectionId                                                   = SectionId.fromByte(5)
    override def indexSpace(componentInstance: ComponentInstance): ComponentIndexSpace = ComponentIndexSpace.Instance
    override def allowsGrouping: Boolean                                               = true
  }

  object ComponentAliasSection extends SectionType[ComponentIndexSpace] {
    override type Section = Alias
    def ct: ClassTag[Section]                                  = implicitly[ClassTag[Alias]]
    override def binaryId: SectionId                           = SectionId.fromByte(6)
    override def indexSpace(alias: Alias): ComponentIndexSpace =
      alias match {
        case Alias.InstanceExport(ComponentExternalKind.Component, _, _) =>
          ComponentIndexSpace.Component
        case Alias.InstanceExport(ComponentExternalKind.Instance, _, _)  =>
          ComponentIndexSpace.Instance
        case Alias.InstanceExport(ComponentExternalKind.Module, _, _)    =>
          ComponentIndexSpace.Module
        case Alias.InstanceExport(ComponentExternalKind.Type, _, _)      =>
          ComponentIndexSpace.Type
        case Alias.InstanceExport(ComponentExternalKind.Func, _, _)      =>
          ComponentIndexSpace.Func
        case Alias.InstanceExport(ComponentExternalKind.Value, _, _)     =>
          ComponentIndexSpace.Value
        case Alias.CoreInstanceExport(ExportKind.Table, _, _)            =>
          ComponentIndexSpace.CoreTable
        case Alias.CoreInstanceExport(ExportKind.Func, _, _)             =>
          ComponentIndexSpace.CoreFunc
        case Alias.CoreInstanceExport(ExportKind.Global, _, _)           =>
          ComponentIndexSpace.CoreGlobal
        case Alias.CoreInstanceExport(ExportKind.Mem, _, _)              =>
          ComponentIndexSpace.CoreMem
        case Alias.Outer(OuterAliasKind.Type, _)                         =>
          ComponentIndexSpace.Type
        case Alias.Outer(OuterAliasKind.CoreType, _)                     =>
          ComponentIndexSpace.CoreType
        case Alias.Outer(OuterAliasKind.Component, _)                    =>
          ComponentIndexSpace.Component
        case Alias.Outer(OuterAliasKind.CoreModule, _)                   =>
          ComponentIndexSpace.Module
      }
    override def allowsGrouping: Boolean                       = true
  }

  object ComponentTypeSection extends SectionType[ComponentIndexSpace] {
    override type Section = ComponentType
    def ct: ClassTag[Section]                                                  = implicitly[ClassTag[ComponentType]]
    override def binaryId: SectionId                                           = SectionId.fromByte(7)
    override def indexSpace(componentType: ComponentType): ComponentIndexSpace = ComponentIndexSpace.Type
    override def allowsGrouping: Boolean                                       = true
  }

  object ComponentCanonSection extends SectionType[ComponentIndexSpace] {
    override type Section = Canon
    def ct: ClassTag[Section]                                  = implicitly[ClassTag[Canon]]
    override def binaryId: SectionId                           = SectionId.fromByte(8)
    override def indexSpace(canon: Canon): ComponentIndexSpace =
      canon match {
        case Canon.Lift(funcIdx, opts, functionType) => ComponentIndexSpace.Func
        case Canon.Lower(funcIdx, opts)              => ComponentIndexSpace.CoreFunc
        case Canon.ResourceNew(typeIdx)              => ComponentIndexSpace.Func
        case Canon.ResourceDrop(typ)                 => ComponentIndexSpace.Func
        case Canon.ResourceRep(typeIdx)              => ComponentIndexSpace.Func
      }

    override def allowsGrouping: Boolean = true
  }

  object ComponentStartSection extends SectionType[ComponentIndexSpace] {
    override type Section = ComponentStart
    def ct: ClassTag[Section]                                            = implicitly[ClassTag[ComponentStart]]
    override def binaryId: SectionId                                     = SectionId.fromByte(9)
    override def indexSpace(starts: ComponentStart): ComponentIndexSpace = ComponentIndexSpace.Start
    override def allowsGrouping: Boolean                                 = false
  }

  object ComponentImportSection extends SectionType[ComponentIndexSpace] {
    override type Section = ComponentImport
    def ct: ClassTag[Section]                                               = implicitly[ClassTag[ComponentImport]]
    override def binaryId: SectionId                                        = SectionId.fromByte(10)
    override def indexSpace(`import`: ComponentImport): ComponentIndexSpace =
      `import` match {
        case ComponentImport(_, ExternDesc.Component(_)) =>
          ComponentIndexSpace.Component
        case ComponentImport(_, ExternDesc.Type(_))      =>
          ComponentIndexSpace.Type
        case ComponentImport(_, ExternDesc.Module(_))    =>
          ComponentIndexSpace.Module
        case ComponentImport(_, ExternDesc.Func(_))      =>
          ComponentIndexSpace.Func
        case ComponentImport(_, ExternDesc.Instance(_))  =>
          ComponentIndexSpace.Instance
        case ComponentImport(name, ExternDesc.Val(_))    =>
          ComponentIndexSpace.Value
      }
    override def allowsGrouping: Boolean                                    = true
  }

  object ComponentExportSection extends SectionType[ComponentIndexSpace] {
    override type Section = ComponentExport
    def ct: ClassTag[Section]                                               = implicitly[ClassTag[ComponentExport]]
    override def binaryId: SectionId                                        = SectionId.fromByte(11)
    override def indexSpace(`export`: ComponentExport): ComponentIndexSpace =
      `export`.kind match {
        case ComponentExternalKind.Component => ComponentIndexSpace.Component
        case ComponentExternalKind.Type      => ComponentIndexSpace.Type
        case ComponentExternalKind.Module    => ComponentIndexSpace.Module
        case ComponentExternalKind.Func      => ComponentIndexSpace.Func
        case ComponentExternalKind.Instance  => ComponentIndexSpace.Instance
        case ComponentExternalKind.Value     => ComponentIndexSpace.Value
      }

    override def allowsGrouping: Boolean = true
  }

  object ComponentCustomSection extends SectionType[ComponentIndexSpace] {
    type Section = Custom

    def ct: ClassTag[Section] = implicitly[ClassTag[Custom]]

    override def binaryId: SectionId = SectionId.fromByte(0)

    override def indexSpace(section: Custom): ComponentIndexSpace = ComponentIndexSpace.Custom

    override def allowsGrouping: Boolean = false
  }
}
