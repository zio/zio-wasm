package zio.wasm.componentmodel

import zio.wasm.*

sealed trait ComponentIndexSpace extends IndexSpace
object ComponentIndexSpace {
  object Func extends ComponentIndexSpace {
    override type Idx = ComponentFuncIdx

    override def fromInt(idx: Int): ComponentFuncIdx =
      ComponentFuncIdx.fromInt(idx)
  }

  object CoreType extends ComponentIndexSpace {
    override type Idx = TypeIdx

    override def fromInt(idx: Int): TypeIdx =
      TypeIdx.fromInt(idx)
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

  object CoreInstance extends ComponentIndexSpace {
    override type Idx = CoreInstanceIdx

    override def fromInt(idx: Int): CoreInstanceIdx =
      CoreInstanceIdx.fromInt(idx)
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

  object CoreTable extends ComponentIndexSpace {
    override type Idx = TableIdx

    override def fromInt(idx: Int): TableIdx =
      TableIdx.fromInt(idx)
  }

  object CoreFunc extends ComponentIndexSpace {
    override type Idx = FuncIdx

    override def fromInt(idx: Int): FuncIdx =
      FuncIdx.fromInt(idx)
  }

  object CoreGlobal extends ComponentIndexSpace {
    override type Idx = GlobalIdx

    override def fromInt(idx: Int): GlobalIdx =
      GlobalIdx.fromInt(idx)
  }

  object CoreMem extends ComponentIndexSpace {
    override type Idx = MemIdx

    override def fromInt(idx: Int): MemIdx =
      MemIdx.fromInt(idx)
  }

  object Canon extends ComponentIndexSpace {
    override type Idx = CanonIdx

    override def fromInt(idx: Int): CanonIdx =
      CanonIdx.fromInt(idx)
  }

  object Start extends ComponentIndexSpace {
    override type Idx = StartIdx

    override def fromInt(idx: Int): StartIdx =
      StartIdx.fromInt(idx)
  }

  object Export extends ComponentIndexSpace {
    type Idx = ExportIdx

    override def fromInt(idx: Int): ExportIdx = ExportIdx.fromInt(idx)
  }

  object Custom extends ComponentIndexSpace {
    type Idx = CustomIdx

    def fromInt(idx: Int): CustomIdx = CustomIdx.fromInt(idx)
  }
}
