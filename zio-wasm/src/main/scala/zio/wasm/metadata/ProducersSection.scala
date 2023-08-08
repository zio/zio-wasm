package zio.wasm.metadata

import zio.Chunk
import zio.parser.Parser.ParserError
import zio.wasm.*
import zio.wasm.SectionType.CustomSection
import zio.wasm.componentmodel.*
import zio.wasm.componentmodel.ComponentSectionType.*
import zio.wasm.metadata.syntax.Binary
import zio.wasm.syntax.SyntaxError

final case class ProducersSection(fields: Chunk[ProducersSectionField])

object ProducersSection {
  val customSectionName: Name = Name.fromString("producers")

  def fromBinary(data: Chunk[Byte]): Either[ParserError[SyntaxError], ProducersSection] =
    Binary.producersSection.parseChunk(data)
}

final case class ProducersSectionField(name: Name, values: Chunk[VersionedName])

final case class VersionedName(name: Name, version: Name)

extension (module: Module) {

  /** Gets the producers section of this module if there is any */
  def producerSection: Option[ProducersSection] =
    module.sections
      .filterBySectionType(CustomSection)
      .find(_.name == ProducersSection.customSectionName)
      .flatMap(section => ProducersSection.fromBinary(section.data).toOption)
}

extension (component: Component) {

  /** Gets the producers section of this component if there is any */
  def producerSection: Option[ProducersSection] =
    component.sections
      .filterBySectionType(ComponentCustomSection)
      .find(_.name == ProducersSection.customSectionName)
      .flatMap(section => ProducersSection.fromBinary(section.data).toOption)

  /** Collects all the producers sections from this component and its subsections */
  def allProducerSections: Chunk[ProducersSection] = {
    val subModules    = component.sections.filterBySectionType(ComponentModuleSection)
    val subComponents = component.sections.filterBySectionType(ComponentSection)

    Chunk.fromIterable(producerSection) ++
      subModules.flatMap(module => Chunk.fromIterable(module.producerSection)) ++
      subComponents.flatMap(component => component.allProducerSections)
  }
}
