package zio.wasm.componentmodel

import zio.wasm.*
import zio.wasm.syntax.Binary.SectionId

import scala.reflect.ClassTag

object ComponentSectionType {
  //    instances: Chunk[Instance],
  //    coreTypes: Chunk[CoreType],
  //    components: Chunk[Component],
  //    componentInstances: Chunk[ComponentInstance],
  //    aliases: Chunk[Alias],
  //    types: Chunk[ComponentType],
  //    canons: Chunk[Canon],
  //    starts: Chunk[ComponentStart],
  //    imports: Chunk[ComponentImport],
  //    exports: Chunk[ComponentExport],
  //    custom: Chunk[Custom]

  object ComponentModuleSection extends SectionType[ComponentIndexSpace] {
    override type Section = Module
    def ct: ClassTag[Section]                                    = implicitly[ClassTag[Module]]
    override def binaryId: SectionId                             = SectionId.fromByte(1)
    override def indexSpace(module: Module): ComponentIndexSpace = ComponentIndexSpace.Module
    override def allowsGrouping: Boolean                         = false
  }

}
