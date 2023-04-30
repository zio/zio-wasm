package zio.wasm

sealed trait CoreIndexSpace extends IndexSpace

object CoreIndexSpace {
  object Type   extends CoreIndexSpace {
    type Idx = TypeIdx
    override def fromInt(idx: Int): TypeIdx = TypeIdx.fromInt(idx)
  }
  object Func   extends CoreIndexSpace {
    type Idx = FuncIdx
    override def fromInt(idx: Int): FuncIdx = FuncIdx.fromInt(idx)
  }
  object Table  extends CoreIndexSpace {
    type Idx = TableIdx
    override def fromInt(idx: Int): TableIdx = TableIdx.fromInt(idx)
  }
  object Mem    extends CoreIndexSpace {
    type Idx = MemIdx
    override def fromInt(idx: Int): MemIdx = MemIdx.fromInt(idx)
  }
  object Global extends CoreIndexSpace {
    type Idx = GlobalIdx
    override def fromInt(idx: Int): GlobalIdx = GlobalIdx.fromInt(idx)
  }
  object Elem   extends CoreIndexSpace {
    type Idx = ElemIdx
    override def fromInt(idx: Int): ElemIdx = ElemIdx.fromInt(idx)
  }
  object Data   extends CoreIndexSpace {
    type Idx = DataIdx
    override def fromInt(idx: Int): DataIdx = DataIdx.fromInt(idx)
  }
  object Local  extends CoreIndexSpace {
    type Idx = LocalIdx
    override def fromInt(idx: Int): LocalIdx = LocalIdx.fromInt(idx)
  }
  object Label  extends CoreIndexSpace {
    type Idx = LabelIdx
    override def fromInt(idx: Int): LabelIdx = LabelIdx.fromInt(idx)
  }

  object Code extends CoreIndexSpace {
    type Idx = FuncIdx
    override def fromInt(idx: Int): FuncIdx = FuncIdx.fromInt(idx)
  }

  object Export extends CoreIndexSpace {
    type Idx = ExportIdx
    override def fromInt(idx: Int): ExportIdx = ExportIdx.fromInt(idx)
  }

  object Start extends CoreIndexSpace {
    type Idx = Unit
    override def fromInt(idx: Int): Unit = ()
  }

  object Custom extends CoreIndexSpace {
    type Idx = CustomIdx
    def fromInt(idx: Int): CustomIdx = CustomIdx.fromInt(idx)
  }
}
