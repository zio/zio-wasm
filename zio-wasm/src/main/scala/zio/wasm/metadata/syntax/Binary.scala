package zio.wasm.metadata.syntax

import zio.parser.*
import zio.wasm.internal.BinarySyntax.BinarySyntax
import zio.wasm.metadata.{ProducersSection, ProducersSectionField, VersionedName}
import zio.wasm.syntax.Binary.{name, vec}

private[wasm] object Binary {
  private val versionedName: BinarySyntax[VersionedName] =
    (name ~ name).of[VersionedName]

  private val producersSectionField: BinarySyntax[ProducersSectionField] =
    (name ~ vec(versionedName)).of[ProducersSectionField]

  val producersSection: BinarySyntax[ProducersSection] =
    vec(producersSectionField).of[ProducersSection]
}
