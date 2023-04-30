package zio.wasm

import zio.wasm.syntax.Binary.SectionId

trait SectionType[IS <: IndexSpace] {
  type Section <: zio.wasm.Section[IS]

  def binaryId: SectionId
  def indexSpace: IS
  def allowsGrouping: Boolean
}

object SectionType {
  object CoreTypeSection extends SectionType[CoreIndexSpace] {
    type Section = FuncType

    override def binaryId: SectionId        = SectionId.fromByte(1)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Type
    override def allowsGrouping: Boolean    = true
  }

  object CoreImportSection extends SectionType[CoreIndexSpace] {
    type Section = Import

    override def binaryId: SectionId        = SectionId.fromByte(2)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Func
    override def allowsGrouping: Boolean    = true
  }

  object CoreFuncSection extends SectionType[CoreIndexSpace] {
    type Section = FuncTypeRef
    override def binaryId: SectionId        = SectionId.fromByte(3)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Func
    override def allowsGrouping: Boolean    = true
  }

  object CoreTableSection extends SectionType[CoreIndexSpace] {
    type Section = Table
    override def binaryId: SectionId        = SectionId.fromByte(4)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Table
    override def allowsGrouping: Boolean    = true
  }

  object CoreMemSection extends SectionType[CoreIndexSpace] {
    type Section = Mem
    override def binaryId: SectionId        = SectionId.fromByte(5)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Mem
    override def allowsGrouping: Boolean    = true
  }

  object CoreGlobalSection extends SectionType[CoreIndexSpace] {
    type Section = Global
    override def binaryId: SectionId        = SectionId.fromByte(6)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Global
    override def allowsGrouping: Boolean    = true
  }

  object CoreExportSection extends SectionType[CoreIndexSpace] {
    type Section = Export
    override def binaryId: SectionId        = SectionId.fromByte(7)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Export
    override def allowsGrouping: Boolean    = true
  }

  object CoreStartSection extends SectionType[CoreIndexSpace] {
    type Section = Start
    override def binaryId: SectionId        = SectionId.fromByte(8)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Start
    override def allowsGrouping: Boolean    = false
  }

  object CoreElemSection extends SectionType[CoreIndexSpace] {
    type Section = Elem
    override def binaryId: SectionId        = SectionId.fromByte(9)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Elem
    override def allowsGrouping: Boolean    = true
  }

  object CoreCodeSection extends SectionType[CoreIndexSpace] {
    type Section = FuncCode
    override def binaryId: SectionId        = SectionId.fromByte(10)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Code
    override def allowsGrouping: Boolean    = true
  }

  object CoreDataSection extends SectionType[CoreIndexSpace] {
    type Section = Data
    override def binaryId: SectionId        = SectionId.fromByte(11)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Data
    override def allowsGrouping: Boolean    = true
  }

  object CoreDataCountSection extends SectionType[CoreIndexSpace] {
    type Section = DataCount
    override def binaryId: SectionId        = SectionId.fromByte(12)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Data
    override def allowsGrouping: Boolean    = false
  }

  object CustomSection extends SectionType[CoreIndexSpace] {
    type Section = Custom
    override def binaryId: SectionId        = SectionId.fromByte(0)
    override def indexSpace: CoreIndexSpace = CoreIndexSpace.Custom
    override def allowsGrouping: Boolean    = false
  }
}
