package zio.wasm

import zio.wasm.componentmodel.SectionReference
import zio.{Chunk, ChunkBuilder}

import scala.annotation.tailrec

final case class Sections[IS <: IndexSpace](sections: Chunk[Section[IS]]) {

  def addToBeginning[S <: Section[IS]](newSection: S): Sections[IS] =
    Sections(newSection +: sections)

  def addToEnd[S <: Section[IS]](newSection: S): Sections[IS] =
    Sections(sections :+ newSection)

  def addToFirstGroupStart[S <: Section[IS]](newSection: S): Sections[IS] = {
    val firstOfType = sections.indexWhere(_.sectionType == newSection.sectionType)
    if (firstOfType == -1) {
      addToBeginning(newSection)
    } else {
      Sections((sections.take(firstOfType) :+ newSection) ++ sections.drop(firstOfType))
    }
  }

  def addToLastGroup[S <: Section[IS]](newSection: S): Sections[IS] = {
    val lastOfType = sections.lastIndexWhere(_.sectionType == newSection.sectionType)
    if (lastOfType == -1) {
      addToEnd(newSection)
    } else {
      Sections((sections.take(lastOfType + 1) :+ newSection) ++ sections.drop(lastOfType + 1))
    }
  }

  def indexed(indexSpace: IS): Map[indexSpace.Idx, Section[IS]] =
    filterByIndexSpace(indexSpace).zipWithIndex.map { case (section, idx) =>
      (indexSpace.fromInt(idx), section)
    }.toMap

  def filterByIndexSpace(indexSpace: IS): Chunk[Section[IS]] =
    sections.filter(section => section.sectionType.tryGetIndexSpace(section) == Some(indexSpace))

  def filterBySectionType(sectionType: SectionType[IS]): Chunk[sectionType.Section] =
    sections.filter(_.sectionType == sectionType).map(_.asInstanceOf[sectionType.Section])

  def map(f: Section[IS] => Section[IS]): Sections[IS] =
    Sections(sections.map(f))

  def mapSectionBySectionType(
      sectionType: SectionType[IS]
  )(f: sectionType.Section => sectionType.Section): Sections[IS] =
    Sections(sections.map {
      case section if section.sectionType == sectionType => f(section.asInstanceOf[sectionType.Section])
      case section                                       => section
    })

  def moveToEnd(section: Section[IS]): Sections[IS] =
    Sections(
      sections.filterNot(_ == section) :+ section
    )

  def replace[S <: Section[IS]](indexSpace: IS)(idx: indexSpace.Idx, section: S): Sections[IS] = {
    assert(section ne null)
    val indexedSections = sections.zipWithIndex.filter { case (section, _) =>
      section.sectionType.tryGetIndexSpace(section) == Some(indexSpace)
    }
    val (_, sectionIdx) = indexedSections(indexSpace.toInt(idx))
    val updatedSections = sections.updated(sectionIdx, section)
    Sections(updatedSections)
  }

  def toGrouped: Chunk[(SectionType[IS], Chunk[Section[IS]])] = {
    @tailrec
    def loop(pos: Int, current: Chunk[Section[IS]], result: Chunk[Chunk[Section[IS]]]): Chunk[Chunk[Section[IS]]] =
      if (pos < sections.length) {
        val next = sections(pos)
        if (current.isEmpty) {
          loop(pos + 1, Chunk(next), result)
        } else {
          val last = current.last
          if (last.sectionType == next.sectionType && last.sectionType.allowsGrouping) {
            loop(pos + 1, current :+ next, result)
          } else {
            loop(pos + 1, Chunk(next), result :+ current)
          }
        }
      } else {
        if (current.isEmpty) result else result :+ current
      }

    loop(0, Chunk.empty, Chunk.empty)
      .map(group => (group.head.sectionType, group))
  }
}

object Sections {
  def fromGrouped[IS <: IndexSpace](sections: Chunk[Chunk[Section[IS]]]): Sections[IS] =
    Sections(sections.flatten)
}
