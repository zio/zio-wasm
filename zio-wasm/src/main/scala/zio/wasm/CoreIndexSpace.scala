package zio.wasm

sealed trait CoreIndexSpace extends IndexSpace

object CoreIndexSpace {
  object Type   extends CoreIndexSpace {
    type Idx = TypeIdx
    override def fromInt(idx: Int): TypeIdx = TypeIdx.fromInt(idx)
    override def toInt(idx: TypeIdx): Int   = idx.toInt
  }
  object Func   extends CoreIndexSpace {
    type Idx = FuncIdx
    override def fromInt(idx: Int): FuncIdx = FuncIdx.fromInt(idx)
    override def toInt(idx: FuncIdx): Int   = idx.toInt
  }
  object Table  extends CoreIndexSpace {
    type Idx = TableIdx
    override def fromInt(idx: Int): TableIdx = TableIdx.fromInt(idx)
    override def toInt(idx: TableIdx): Int   = idx.toInt
  }
  object Mem    extends CoreIndexSpace {
    type Idx = MemIdx
    override def fromInt(idx: Int): MemIdx = MemIdx.fromInt(idx)
    override def toInt(idx: MemIdx): Int   = idx.toInt
  }
  object Global extends CoreIndexSpace {
    type Idx = GlobalIdx
    override def fromInt(idx: Int): GlobalIdx = GlobalIdx.fromInt(idx)
    override def toInt(idx: GlobalIdx): Int   = idx.toInt
  }
  object Elem   extends CoreIndexSpace {
    type Idx = ElemIdx
    override def fromInt(idx: Int): ElemIdx = ElemIdx.fromInt(idx)
    override def toInt(idx: ElemIdx): Int   = idx.toInt
  }
  object Data   extends CoreIndexSpace {
    type Idx = DataIdx
    override def fromInt(idx: Int): DataIdx = DataIdx.fromInt(idx)
    override def toInt(idx: DataIdx): Int   = idx.toInt
  }
  object Local  extends CoreIndexSpace {
    type Idx = LocalIdx
    override def fromInt(idx: Int): LocalIdx = LocalIdx.fromInt(idx)
    override def toInt(idx: LocalIdx): Int   = idx.toInt
  }
  object Label  extends CoreIndexSpace {
    type Idx = LabelIdx
    override def fromInt(idx: Int): LabelIdx = LabelIdx.fromInt(idx)
    override def toInt(idx: LabelIdx): Int   = idx.toInt
  }

  object Code extends CoreIndexSpace {
    type Idx = FuncIdx
    override def fromInt(idx: Int): FuncIdx = FuncIdx.fromInt(idx)
    override def toInt(idx: FuncIdx): Int   = idx.toInt
  }

  object Export extends CoreIndexSpace {
    type Idx = ExportIdx
    override def fromInt(idx: Int): ExportIdx = ExportIdx.fromInt(idx)
    override def toInt(idx: ExportIdx): Int   = idx.toInt
  }

  object Start extends CoreIndexSpace {
    type Idx = Unit
    override def fromInt(idx: Int): Unit = ()
    override def toInt(idx: Unit): Int   = 0
  }

  object Custom extends CoreIndexSpace {
    type Idx = CustomIdx
    def fromInt(idx: Int): CustomIdx = CustomIdx.fromInt(idx)
    def toInt(idx: CustomIdx): Int   = idx.toInt
  }
}
