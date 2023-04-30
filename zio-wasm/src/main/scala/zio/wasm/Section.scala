package zio.wasm

trait Section[IS <: IndexSpace] {
  def sectionType: SectionType[IS]
}
