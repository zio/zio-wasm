package zio.wasm.componentmodel

import zio.wasm.*

sealed trait ComponentIndexSpace extends IndexSpace
object ComponentIndexSpace {
  object Func extends ComponentIndexSpace {
    override type Idx = ComponentFuncIdx

    override def fromInt(idx: Int): ComponentFuncIdx =
      ComponentFuncIdx.fromInt(idx)

    override def toInt(idx: ComponentFuncIdx): Int =
      idx.toInt
  }

  object CoreType extends ComponentIndexSpace {
    override type Idx = TypeIdx

    override def fromInt(idx: Int): TypeIdx =
      TypeIdx.fromInt(idx)

    override def toInt(idx: TypeIdx): Int =
      idx.toInt
  }

  object Type extends ComponentIndexSpace {
    override type Idx = ComponentTypeIdx

    override def fromInt(idx: Int): ComponentTypeIdx =
      ComponentTypeIdx.fromInt(idx)

    override def toInt(idx: ComponentTypeIdx): Int =
      idx.toInt
  }

  object Module extends ComponentIndexSpace {
    override type Idx = ModuleIdx

    override def fromInt(idx: Int): ModuleIdx =
      ModuleIdx.fromInt(idx)

    override def toInt(idx: ModuleIdx): Int =
      idx.toInt
  }

  object Component extends ComponentIndexSpace {
    override type Idx = ComponentIdx

    override def fromInt(idx: Int): ComponentIdx =
      ComponentIdx.fromInt(idx)

    override def toInt(idx: ComponentIdx): Int =
      idx.toInt
  }

  object CoreInstance extends ComponentIndexSpace {
    override type Idx = CoreInstanceIdx

    override def fromInt(idx: Int): CoreInstanceIdx =
      CoreInstanceIdx.fromInt(idx)

    override def toInt(idx: CoreInstanceIdx): Int =
      idx.toInt
  }

  object Instance extends ComponentIndexSpace {
    override type Idx = InstanceIdx

    override def fromInt(idx: Int): InstanceIdx =
      InstanceIdx.fromInt(idx)

    override def toInt(idx: InstanceIdx): Int =
      idx.toInt
  }

  object Value extends ComponentIndexSpace {
    override type Idx = ValueIdx

    override def fromInt(idx: Int): ValueIdx =
      ValueIdx.fromInt(idx)

    override def toInt(idx: ValueIdx): Int =
      idx.toInt
  }

  object CoreTable extends ComponentIndexSpace {
    override type Idx = TableIdx

    override def fromInt(idx: Int): TableIdx =
      TableIdx.fromInt(idx)

    override def toInt(idx: TableIdx): Int =
      idx.toInt
  }

  object CoreFunc extends ComponentIndexSpace {
    override type Idx = FuncIdx

    override def fromInt(idx: Int): FuncIdx =
      FuncIdx.fromInt(idx)

    override def toInt(idx: FuncIdx): Int =
      idx.toInt
  }

  object CoreGlobal extends ComponentIndexSpace {
    override type Idx = GlobalIdx

    override def fromInt(idx: Int): GlobalIdx =
      GlobalIdx.fromInt(idx)

    override def toInt(idx: GlobalIdx): Int =
      idx.toInt
  }

  object CoreMem extends ComponentIndexSpace {
    override type Idx = MemIdx

    override def fromInt(idx: Int): MemIdx =
      MemIdx.fromInt(idx)

    override def toInt(idx: MemIdx): Int =
      idx.toInt
  }

  object Start extends ComponentIndexSpace {
    override type Idx = StartIdx

    override def fromInt(idx: Int): StartIdx =
      StartIdx.fromInt(idx)

    override def toInt(idx: StartIdx): Int =
      idx.toInt
  }

  object Custom extends ComponentIndexSpace {
    type Idx = CustomIdx

    def fromInt(idx: Int): CustomIdx = CustomIdx.fromInt(idx)
    def toInt(idx: CustomIdx): Int   = idx.toInt
  }
}
