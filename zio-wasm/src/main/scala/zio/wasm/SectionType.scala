package zio.wasm

import zio.wasm.syntax.Binary.SectionId

import scala.reflect.ClassTag

trait SectionType[IS <: IndexSpace] {

  /** The actual section model type in the AST */
  type Section <: zio.wasm.Section[IS]

  def ct: ClassTag[Section]

  implicit private def ict: ClassTag[Section] = ct

  /** The binary section ID */
  def binaryId: SectionId

  /**
   * Index space - can depend on the actual value.
   *
   * An example is component model's import and alias sections, which can refer to elements of various other index
   * spaces.
   */
  def indexSpace(section: Section): IS

  def tryGetIndexSpace(anySection: zio.wasm.Section[IS]): Option[IS] =
    anySection match {
      case section: Section => Some(indexSpace(section))
      case _                => None
    }

  /**
   * If true, sections of this type can be grouped together and serialized as a vector. If false, each section is
   * serialized as a separate binary section.
   */
  def allowsGrouping: Boolean
}

object SectionType {

  object CoreTypeSection extends SectionType[CoreIndexSpace] {
    type Section = FuncType
    def ct: ClassTag[Section] = implicitly[ClassTag[FuncType]]

    override def binaryId: SectionId                           = SectionId.fromByte(1)
    override def indexSpace(section: FuncType): CoreIndexSpace = CoreIndexSpace.Type
    override def allowsGrouping: Boolean                       = true
  }

  object CoreImportSection extends SectionType[CoreIndexSpace] {
    type Section = Import
    def ct: ClassTag[Section] = implicitly[ClassTag[Import]]

    override def binaryId: SectionId                         = SectionId.fromByte(2)
    override def indexSpace(section: Import): CoreIndexSpace = CoreIndexSpace.Func
    override def allowsGrouping: Boolean                     = true
  }

  object CoreFuncSection extends SectionType[CoreIndexSpace] {
    type Section = FuncTypeRef
    def ct: ClassTag[Section] = implicitly[ClassTag[FuncTypeRef]]

    override def binaryId: SectionId                              = SectionId.fromByte(3)
    override def indexSpace(section: FuncTypeRef): CoreIndexSpace = CoreIndexSpace.Func
    override def allowsGrouping: Boolean                          = true
  }

  object CoreTableSection extends SectionType[CoreIndexSpace] {
    type Section = Table
    def ct: ClassTag[Section]                               = implicitly[ClassTag[Table]]
    override def binaryId: SectionId                        = SectionId.fromByte(4)
    override def indexSpace(section: Table): CoreIndexSpace = CoreIndexSpace.Table
    override def allowsGrouping: Boolean                    = true
  }

  object CoreMemSection extends SectionType[CoreIndexSpace] {
    type Section = Mem
    def ct: ClassTag[Section]                             = implicitly[ClassTag[Mem]]
    override def binaryId: SectionId                      = SectionId.fromByte(5)
    override def indexSpace(section: Mem): CoreIndexSpace = CoreIndexSpace.Mem
    override def allowsGrouping: Boolean                  = true
  }

  object CoreGlobalSection extends SectionType[CoreIndexSpace] {
    type Section = Global
    def ct: ClassTag[Section]                                = implicitly[ClassTag[Global]]
    override def binaryId: SectionId                         = SectionId.fromByte(6)
    override def indexSpace(section: Global): CoreIndexSpace = CoreIndexSpace.Global
    override def allowsGrouping: Boolean                     = true
  }

  object CoreExportSection extends SectionType[CoreIndexSpace] {
    type Section = Export
    def ct: ClassTag[Section]                                = implicitly[ClassTag[Export]]
    override def binaryId: SectionId                         = SectionId.fromByte(7)
    override def indexSpace(section: Export): CoreIndexSpace = CoreIndexSpace.Export
    override def allowsGrouping: Boolean                     = true
  }

  object CoreStartSection extends SectionType[CoreIndexSpace] {
    type Section = Start
    def ct: ClassTag[Section] = implicitly[ClassTag[Start]]

    override def binaryId: SectionId                        = SectionId.fromByte(8)
    override def indexSpace(section: Start): CoreIndexSpace = CoreIndexSpace.Start
    override def allowsGrouping: Boolean                    = false
  }

  object CoreElemSection extends SectionType[CoreIndexSpace] {
    type Section = Elem
    def ct: ClassTag[Section]                              = implicitly[ClassTag[Elem]]
    override def binaryId: SectionId                       = SectionId.fromByte(9)
    override def indexSpace(section: Elem): CoreIndexSpace = CoreIndexSpace.Elem
    override def allowsGrouping: Boolean                   = true
  }

  object CoreCodeSection extends SectionType[CoreIndexSpace] {
    type Section = FuncCode
    def ct: ClassTag[Section]                                  = implicitly[ClassTag[FuncCode]]
    override def binaryId: SectionId                           = SectionId.fromByte(10)
    override def indexSpace(section: FuncCode): CoreIndexSpace = CoreIndexSpace.Code
    override def allowsGrouping: Boolean                       = true
  }

  object CoreDataSection extends SectionType[CoreIndexSpace] {
    type Section = Data
    def ct: ClassTag[Section]                              = implicitly[ClassTag[Data]]
    override def binaryId: SectionId                       = SectionId.fromByte(11)
    override def indexSpace(section: Data): CoreIndexSpace = CoreIndexSpace.Data
    override def allowsGrouping: Boolean                   = true
  }

  object CoreDataCountSection extends SectionType[CoreIndexSpace] {
    type Section = DataCount
    def ct: ClassTag[Section]                                   = implicitly[ClassTag[DataCount]]
    override def binaryId: SectionId                            = SectionId.fromByte(12)
    override def indexSpace(section: DataCount): CoreIndexSpace = CoreIndexSpace.Data
    override def allowsGrouping: Boolean                        = false
  }

  object CustomSection extends SectionType[CoreIndexSpace] {
    type Section = Custom
    def ct: ClassTag[Section] = implicitly[ClassTag[Custom]]

    override def binaryId: SectionId                         = SectionId.fromByte(0)
    override def indexSpace(section: Custom): CoreIndexSpace = CoreIndexSpace.Custom
    override def allowsGrouping: Boolean                     = false
  }
}
