package zio.wasm.componentmodel

import zio.wasm.IndexSpace

sealed trait ComponentIndexSpace extends IndexSpace
object ComponentIndexSpace {
  object Func extends ComponentIndexSpace {
    override type Idx = ComponentFuncIdx

    override def fromInt(idx: Int): ComponentFuncIdx =
      ComponentFuncIdx.fromInt(idx)
  }

  object Type extends ComponentIndexSpace {
    override type Idx = ComponentTypeIdx

    override def fromInt(idx: Int): ComponentTypeIdx =
      ComponentTypeIdx.fromInt(idx)
  }

  object Module extends ComponentIndexSpace {
    override type Idx = ModuleIdx

    override def fromInt(idx: Int): ModuleIdx =
      ModuleIdx.fromInt(idx)
  }

  object Component extends ComponentIndexSpace {
    override type Idx = ComponentIdx

    override def fromInt(idx: Int): ComponentIdx =
      ComponentIdx.fromInt(idx)
  }

  object Instance extends ComponentIndexSpace {
    override type Idx = InstanceIdx

    override def fromInt(idx: Int): InstanceIdx =
      InstanceIdx.fromInt(idx)
  }

  object Value extends ComponentIndexSpace {
    override type Idx = ValueIdx

    override def fromInt(idx: Int): ValueIdx =
      ValueIdx.fromInt(idx)
  }
}
