package zio.wasm.componentmodel

import zio.wasm.TypeIdx
import zio.Chunk

import scala.reflect.ClassTag

// Combines all the different section ids to a single type
enum SectionReference {
  case CoreType(idx: TypeIdx)
  case ComponentType(idx: ComponentTypeIdx)
  case ComponentFunc(idx: ComponentFuncIdx)
  case Module(idx: ModuleIdx)
  case Component(idx: ComponentIdx)
  case CoreInstance(idx: CoreInstanceIdx)
  case Instance(idx: InstanceIdx)
  case Value(idx: ValueIdx)
  case Start(idx: StartIdx)
}

object SectionReference {
  trait Mapper {
    def map[S <: SectionReference: ClassTag](value: S): S
  }

  object Mapper {
    def fromPairs(pairs: Chunk[(SectionReference, SectionReference)]): Mapper =
      new Mapper {
        def map[S <: SectionReference: ClassTag](value: S): S =
          pairs
            .collectFirst {
              case ((from: S, to: S)) if from == value => to
            }
            .getOrElse(value)
      }
  }
}
