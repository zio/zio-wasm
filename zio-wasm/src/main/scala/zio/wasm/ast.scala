package zio.wasm

import zio.*

import java.nio.charset.StandardCharsets

/** WebAssembly programs are organized into modules, which are the unit of deployment, loading, and compilation. A
  * module collects definitions for types, functions, tables, memories, and globals. In addition, it can declare imports
  * and exports and provide initialization in the form of data and element segments, or a start function.
  */
final case class Module(
    types: Chunk[FuncType],
    funcs: Chunk[Func],
    tables: Chunk[Table],
    mems: Chunk[Mem],
    globals: Chunk[Global],
    elems: Chunk[Elem],
    datas: Chunk[Data],
    start: Option[Start],
    imports: Chunk[Import],
    exports: Chunk[Export],
    custom: Chunk[Custom]
) {

  def addFunction(funcType: FuncType, locals: Chunk[ValType], body: Expr): (Module, FuncIdx) = {
    val existingIndex         = types.indexOf(funcType)
    val (typeIndex, newTypes) =
      if (existingIndex >= 0) (TypeIdx.fromInt(existingIndex), types)
      else (TypeIdx.fromInt(types.length), types :+ funcType)
    val func                  = Func(typeIndex, locals, body)
    val funcIdx               = FuncIdx.fromInt(funcs.length)
    (
      this.copy(
        types = newTypes,
        funcs = funcs :+ func
      ),
      funcIdx
    )
  }

  def addTable(table: Table): (Module, TableIdx) =
    (
      this.copy(tables = tables :+ table),
      TableIdx.fromInt(tables.length)
    )

  lazy val importedFunctions: Map[FuncIdx, (Name, Name, FuncType)] =
    imports
      .collect { case Import(module, name, ImportDesc.Func(funcTypeIdx)) =>
        (module, name, types(funcTypeIdx.toInt))
      }
      .zipWithIndex
      .map { (desc, idx) =>
        (FuncIdx.fromInt(idx), desc)
      }
      .toMap

  lazy val firstLocalFunctionIndex: FuncIdx = importedFunctions.lastOption.map(_._1.next).getOrElse(FuncIdx.fromInt(0))

  def foldLeftRec[S](initial: S)(f: (S, Instr) => S): S =
    funcs.foldLeft(initial) { case (s, func) => func.foldLeftRec(s)(f) }

  def mapInstr(f: Instr => Instr): Module =
    this.copy(
      funcs = funcs.map(_.mapInstr(f))
    )
}

// Indices

// Definitions are referenced with zero-based indices. Each class of definition has its own index space, as distinguished by the following classes.
// The index space for functions, tables, memories and globals includes respective imports declared in the same module. The indices of these imports precede the indices of other definitions in the same index space.
//
// Element indices reference element segments and data indices reference data segments.
//
// The index space for locals is only accessible inside a function and includes the parameters of that function, which precede the local variables.
//
// Label indices reference structured control instructions inside an instruction sequence.

type TypeIdx = TypeIdx.TypeIdx
object TypeIdx {
  opaque type TypeIdx = Int
  def fromInt(value: Int): TypeIdx = value

  extension (idx: TypeIdx) {
    def toInt: Int = idx
  }
}

type FuncIdx = FuncIdx.FuncIdx
object FuncIdx {
  opaque type FuncIdx = Int
  def fromInt(value: Int): FuncIdx = value

  extension (idx: FuncIdx) {
    def next: FuncIdx = idx + 1
    def toInt: Int    = idx
  }
}

type TableIdx = TableIdx.TableIdx
object TableIdx {
  opaque type TableIdx = Int
  def fromInt(value: Int): TableIdx = value

  extension (idx: TableIdx) {
    def toInt: Int = idx
  }
}

type MemIdx = MemIdx.MemIdx
object MemIdx {
  opaque type MemIdx = Int
  def fromInt(value: Int): MemIdx = value

  extension (idx: MemIdx) {
    def toInt: Int = idx
  }

}

type GlobalIdx = GlobalIdx.GlobalIdx
object GlobalIdx {
  opaque type GlobalIdx = Int
  def fromInt(value: Int): GlobalIdx = value

  extension (idx: GlobalIdx) {
    def toInt: Int = idx
  }
}

type ElemIdx = ElemIdx.ElemIdx
object ElemIdx {
  opaque type ElemIdx = Int
  def fromInt(value: Int): ElemIdx = value

  extension (idx: ElemIdx) {
    def toInt: Int = idx
  }
}

type DataIdx = DataIdx.DataIdx
object DataIdx {
  opaque type DataIdx = Int
  def fromInt(value: Int): DataIdx = value

  extension (idx: DataIdx) {
    def toInt: Int = idx
  }
}

type LocalIdx = LocalIdx.LocalIdx
object LocalIdx {
  opaque type LocalIdx = Int
  def fromInt(value: Int): LocalIdx = value

  extension (idx: LocalIdx) {
    def toInt: Int = idx
  }
}

opaque type LabelIdx = Int
object LabelIdx {
  def fromInt(value: Int): LabelIdx = value

  extension (idx: LabelIdx) {
    def toInt: Int = idx
  }
}

// Types

// The component of a module defines a vector of function types.
//
// All function types used in a module must be defined in this component. They are referenced by type indices.

/** Number types classify numeric values.
  *
  * The types i32 and i64 classify 32 and 64 bit integers, respectively. Integers are not inherently signed or unsigned,
  * their interpretation is determined by individual operations.
  *
  * The types f32 and f64 classify 32 and 64 bit floating-point data, respectively. They correspond to the respective
  * binary floating-point representations, also known as single and double precision, as defined by the IEEE 754
  * standard (Section 3.3).
  *
  * Number types are transparent, meaning that their bit patterns can be observed. Values of number type can be stored
  * in memories.
  */
enum NumType {
  case I32
  case I64
  case F32
  case F64
}

/** Vector types classify vectors of numeric values processed by vector instructions (also known as SIMD instructions,
  * single instruction multiple data).
  *
  * The type v128 corresponds to a 128 bit vector of packed integer or floating-point data. The packed data can be
  * interpreted as signed or unsigned integers, single or double precision floating-point values, or a single 128 bit
  * type. The interpretation is determined by individual operations.
  *
  * Vector types, like number types are transparent, meaning that their bit patterns can be observed. Values of vector
  * type can be stored in memories.
  */
enum VecType {
  case V128
}

/** Reference types classify first-class references to objects in the runtime store.
  *
  * The type funcref denotes the infinite union of all references to functions, regardless of their function types.
  *
  * The type externref denotes the infinite union of all references to objects owned by the embedder and that can be
  * passed into WebAssembly under this type.
  *
  * Reference types are opaque, meaning that neither their size nor their bit pattern can be observed. Values of
  * reference type can be stored in tables.
  */
enum RefType {
  case FuncRef
  case ExternRef
}

/** Value types classify the individual values that WebAssembly code can compute with and the values that a variable
  * accepts. They are either number types, vector types, or reference types.
  */
type ValType = NumType | VecType | RefType

/** Result types classify the result of executing instructions or functions, which is a sequence of values, written with
  * brackets.
  */
final case class ResultType(values: Chunk[ValType])

/** Function types classify the signature of functions, mapping a vector of parameters to a vector of results. They are
  * also used to classify the inputs and outputs of instructions.
  */
final case class FuncType(input: ResultType, output: ResultType)

/** Limits classify the size range of resizeable storage associated with memory types and table types.
  *
  * If no maximum is given, the respective storage can grow to any size.
  */
final case class Limits(min: Int, max: Option[Int])

/** Memory types classify linear memories and their size range.
  *
  * The limits constrain the minimum and optionally the maximum size of a memory. The limits are given in units of page
  * size.
  */
final case class MemType(limits: Limits)

/** Table types classify tables over elements of reference type within a size range.
  *
  * Like memories, tables are constrained by limits for their minimum and optionally maximum size. The limits are given
  * in numbers of entries.
  */
final case class TableType(limits: Limits, elements: RefType)

enum Mut {
  case Const
  case Var
}

/** Global types classify global variables, which hold a value and can either be mutable or immutable.
  */
final case class GlobalType(mut: Mut, valType: ValType)

/** External types classify imports and external values with their respective types.
  */
enum ExternType {
  case Func(funcType: FuncType)
  case Table(tableType: TableType)
  case Mem(memType: MemType)
  case Global(globalType: GlobalType)
}

// Functions

/** The funcs component of a module defines a vector of functions with the following structure.
  *
  * Functions are referenced through function indices, starting with the smallest index not referencing a function
  * import.
  *
  * @param typ
  *   The type of a function declares its signature by reference to a type defined in the module. The parameters of the
  *   function are referenced through 0-based local indices in the function’s body; they are mutable.
  * @param locals
  *   The locals declare a vector of mutable local variables and their types. These variables are referenced through
  *   local indices in the function’s body. The index of the first local is the smallest index not referencing a
  *   parameter.
  * @param body
  *   The body is an instruction sequence that upon termination must produce a stack matching the function type’s result
  *   type.
  */
final case class Func(typ: TypeIdx, locals: Chunk[ValType], body: Expr) {
  def foldLeftRec[S](initial: S)(f: (S, Instr) => S): S =
    body.foldLeftRec(initial)(f)

  def mapInstr(f: Instr => Instr): Func =
    this.copy(body = body.mapInstr(f))
}

// Tables

/** The tables component of a module defines a vector of tables described by their table type:
  *
  * A table is a vector of opaque values of a particular reference type. The size in the limits of the table type
  * specifies the initial size of that table, while its max, if present, restricts the size to which it can grow later.
  *
  * Tables can be initialized through element segments.
  *
  * Tables are referenced through table indices, starting with the smallest index not referencing a table import. Most
  * constructs implicitly reference table index 0.
  */
final case class Table(tableType: TableType)

// Memories

/** The mems component of a module defines a vector of linear memories (or memories for short) as described by their
  * memory type:
  *
  * A memory is a vector of raw uninterpreted bytes. The size in the limits of the memory type specifies the initial
  * size of that memory, while its max, if present, restricts the size to which it can grow later. Both are in units of
  * page size.
  *
  * Memories can be initialized through data segments.
  *
  * Memories are referenced through memory indices, starting with the smallest index not referencing a memory import.
  * Most constructs implicitly reference memory index 0.
  */
final case class Mem(memType: MemType)

// Globals

/** The globals component of a module defines a vector of global variables (or globals for short):
  *
  * Each global stores a single value of the given global type. Its type also specifies whether a global is immutable or
  * mutable. Moreover, each global is initialized with an value given by a constant initializer expression.
  *
  * Globals are referenced through global indices, starting with the smallest index not referencing a global import.
  */
final case class Global(globalType: GlobalType, init: Expr)

// Element segments

enum ElemMode {
  case Passive
  case Active(table: TableIdx, offset: Expr)
  case Declarative
}

/** The initial contents of a table is uninitialized. Element segments can be used to initialize a subrange of a table
  * from a static vector of elements.
  *
  * The elems component of a module defines a vector of element segments. Each element segment defines a reference type
  * and a corresponding list of constant element expressions.
  *
  * Element segments have a mode that identifies them as either passive, active, or declarative. A passive element
  * segment’s elements can be copied to a table using the table.init instruction. An active element segment copies its
  * elements into a table during instantiation, as specified by a table index and a constant expression defining an
  * offset into that table. A declarative element segment is not available at runtime but merely serves to
  * forward-declare references that are formed in code with instructions like ref.func.
  *
  * Element segments are referenced through element indices.
  */
final case class Elem(refType: RefType, init: Chunk[Expr], mode: ElemMode)

// Data segments

enum DataMode {
  case Passive
  case Active(memory: MemIdx, offset: Expr)
}

/** The initial contents of a memory are zero bytes. Data segments can be used to initialize a range of memory from a
  * static vector of bytes.
  *
  * The datas component of a module defines a vector of data segments.
  *
  * Like element segments, data segments have a mode that identifies them as either passive or active. A passive data
  * segment’s contents can be copied into a memory using the memory.init instruction. An active data segment copies its
  * contents into a memory during instantiation, as specified by a memory index and a constant expression defining an
  * offset into that memory.
  *
  * Data segments are referenced through data indices.
  */
final case class Data(init: Chunk[Byte], mode: DataMode)

// Start function

/** The start component of a module declares the function index of a start function that is automatically invoked when
  * the module is instantiated, after tables and memories have been initialized.
  */
final case class Start(func: FuncIdx)

// Exports
enum ExportDesc {
  case Func(funcIdx: FuncIdx)
  case Table(tableIdx: TableIdx)
  case Mem(memIdx: MemIdx)
  case Global(globalIdx: GlobalIdx)
}

/** The exports component of a module defines a set of exports that become accessible to the host environment once the
  * module has been instantiated.
  *
  * Each export is labeled by a unique name. Exportable definitions are functions, tables, memories, and globals, which
  * are referenced through a respective descriptor.
  */
final case class Export(name: Name, desc: ExportDesc)

// Imports

enum ImportDesc {
  case Func(funcIdx: FuncIdx)
  case Table(tableIdx: TableIdx)
  case Mem(memIdx: MemIdx)
  case Global(globalIdx: GlobalIdx)
}

/** The imports component of a module defines a set of imports that are required for instantiation.
  *
  * Each import is labeled by a two-level name space, consisting of a module name and a name for an entity within that
  * module. Importable definitions are functions, tables, memories, and globals. Each import is specified by a
  * descriptor with a respective type that a definition provided during instantiation is required to match.
  *
  * Every import defines an index in the respective index space. In each index space, the indices of imports go before
  * the first index of any definition contained in the module itself.
  */
final case class Import(module: Name, name: Name, desc: ImportDesc)

final case class Custom(name: Name, data: Chunk[Byte])

// Instructions

enum IntWidth {
  case I32
  case I64
}

enum FloatWidth {
  case F32
  case F64
}

enum Signedness {
  case Signed
  case Unsigned
}

enum IShape {
  case I8x16
  case I16x8
  case I32x4
  case I64x2
}

enum FShape {
  case F32x4
  case F64x2
}

type Shape = IShape | FShape

enum Half {
  case Low
  case High
}

type LaneIdx = LaneIdx.LaneIdx
object LaneIdx {
  opaque type LaneIdx = Byte
  def fromByte(b: Byte): LaneIdx = b

  extension (idx: LaneIdx) {
    def toByte: Byte = idx
  }
}

final case class MemArg(align: Int, offset: Int)

enum VectorLoadShape {
  case WW8
  case WW16
  case WW32
  case WW64
}

enum BlockType {
  case None
  case Index(idx: TypeIdx)
  case Value(valType: ValType)
}

/** Numeric instructions provide basic operations over numeric values of specific type. These operations closely match
  * respective operations available in hardware.
  */
sealed trait NumericInstr
sealed trait IRelOp extends NumericInstr
sealed trait FRelOp extends NumericInstr
sealed trait IUnOp  extends NumericInstr
sealed trait IBinOp extends NumericInstr

sealed trait FUnOp   extends NumericInstr
sealed trait FBinOp  extends NumericInstr
sealed trait ITestOp extends NumericInstr
sealed trait CvtOp   extends NumericInstr

type UnOp   = IUnOp | FUnOp
type BinOp  = IBinOp | FBinOp
type TestOp = ITestOp
type RelOp  = IRelOp | FRelOp

/** Vector instructions (also known as SIMD instructions, single instruction multiple data) provide basic operations
  * over values of vector type.
  */
sealed trait VectorInstr
sealed trait VVUnOp   extends VectorInstr
sealed trait VVBinOp  extends VectorInstr
sealed trait VVTernOp extends VectorInstr
sealed trait VVTestOp extends VectorInstr

sealed trait VIRelOp    extends VectorInstr
sealed trait VIUnOp     extends VectorInstr
sealed trait VIBinOp    extends VectorInstr
sealed trait VFRelOp    extends VectorInstr
sealed trait VFUnOp     extends VectorInstr
sealed trait VFBinOp    extends VectorInstr
sealed trait VITestOp   extends VectorInstr
sealed trait VIShiftOp  extends VectorInstr
sealed trait VIMinMaxOp extends VectorInstr
sealed trait VISatBinOp extends VectorInstr
sealed trait VCvtOp     extends VectorInstr

type VUnOp   = VIUnOp | VFUnOp | Instr.VI8x16PopCnt.type
type VBinOp  = VIBinOp | VFBinOp | VIMinMaxOp | VISatBinOp | Instr.VIMul.type | Instr.VFMul.type | Instr.VIAvgr.type |
  Instr.VI16x8Q15MulrSat.type
type VTestOp = VITestOp
type VRelOp  = VIRelOp | VFRelOp

/** Instructions in this group are concerned with accessing references.
  */
sealed trait ReferenceInstr

/** Instructions in this group can operate on operands of any value type.
  */
sealed trait ParametricInstr

/** Variable instructions are concerned with access to local or global variables.
  */
sealed trait VariableInstr

/** Instructions in this group are concerned with tables table.
  */
sealed trait TableInstr

/** Instructions in this group are concerned with linear memory.
  */
sealed trait MemoryInstr

/** Instructions in this group affect the flow of control.
  */
sealed trait ControlInstr

enum Instr {
  // NumericInstr
  case I32Const(value: Int)    extends Instr with NumericInstr
  case I64Const(value: Long)   extends Instr with NumericInstr
  case F32Const(value: Float)  extends Instr with NumericInstr
  case F64Const(value: Double) extends Instr with NumericInstr

  // ITestOp
  case IEqz(width: IntWidth) extends Instr with NumericInstr with ITestOp

  // IRelOp
  case IEq(width: IntWidth)                         extends Instr with NumericInstr with IRelOp
  case INe(width: IntWidth)                         extends Instr with NumericInstr with IRelOp
  case ILt(width: IntWidth, signedness: Signedness) extends Instr with NumericInstr with IRelOp
  case IGt(width: IntWidth, signedness: Signedness) extends Instr with NumericInstr with IRelOp
  case ILe(width: IntWidth, signedness: Signedness) extends Instr with NumericInstr with IRelOp
  case IGe(width: IntWidth, signedness: Signedness) extends Instr with NumericInstr with IRelOp

  // FRelOp
  case FEq(width: FloatWidth) extends Instr with NumericInstr with FRelOp
  case FNe(width: FloatWidth) extends Instr with NumericInstr with FRelOp
  case FLt(width: FloatWidth) extends Instr with NumericInstr with FRelOp
  case FGt(width: FloatWidth) extends Instr with NumericInstr with FRelOp
  case FLe(width: FloatWidth) extends Instr with NumericInstr with FRelOp
  case FGe(width: FloatWidth) extends Instr with NumericInstr with FRelOp

  // IUnOp
  case IClz(width: IntWidth)    extends Instr with NumericInstr with IUnOp
  case ICtz(width: IntWidth)    extends Instr with NumericInstr with IUnOp
  case IPopCnt(width: IntWidth) extends Instr with NumericInstr with IUnOp

  // IBinOp
  case IAdd(width: IntWidth)                         extends Instr with NumericInstr with IBinOp
  case ISub(width: IntWidth)                         extends Instr with NumericInstr with IBinOp
  case IMul(width: IntWidth)                         extends Instr with NumericInstr with IBinOp
  case IDiv(width: IntWidth, signedness: Signedness) extends Instr with NumericInstr with IBinOp
  case IRem(width: IntWidth, signedness: Signedness) extends Instr with NumericInstr with IBinOp
  case IAnd(width: IntWidth)                         extends Instr with NumericInstr with IBinOp
  case IOr(width: IntWidth)                          extends Instr with NumericInstr with IBinOp
  case IXor(width: IntWidth)                         extends Instr with NumericInstr with IBinOp
  case IShl(width: IntWidth)                         extends Instr with NumericInstr with IBinOp
  case IShr(width: IntWidth, signedness: Signedness) extends Instr with NumericInstr with IBinOp
  case IRotL(width: IntWidth)                        extends Instr with NumericInstr with IBinOp
  case IRotR(width: IntWidth)                        extends Instr with NumericInstr with IBinOp

  // FUnOp
  case FAbs(width: FloatWidth)     extends Instr with NumericInstr with FUnOp
  case FNeg(width: FloatWidth)     extends Instr with NumericInstr with FUnOp
  case FCeil(width: FloatWidth)    extends Instr with NumericInstr with FUnOp
  case FFloor(width: FloatWidth)   extends Instr with NumericInstr with FUnOp
  case FTrunc(width: FloatWidth)   extends Instr with NumericInstr with FUnOp
  case FNearest(width: FloatWidth) extends Instr with NumericInstr with FUnOp

  // FBinOp
  case FSqrt(width: FloatWidth)     extends Instr with NumericInstr with FBinOp
  case FAdd(width: FloatWidth)      extends Instr with NumericInstr with FBinOp
  case FSub(width: FloatWidth)      extends Instr with NumericInstr with FBinOp
  case FMul(width: FloatWidth)      extends Instr with NumericInstr with FBinOp
  case FDiv(width: FloatWidth)      extends Instr with NumericInstr with FBinOp
  case FMin(width: FloatWidth)      extends Instr with NumericInstr with FBinOp
  case FMax(width: FloatWidth)      extends Instr with NumericInstr with FBinOp
  case FCopySign(width: FloatWidth) extends Instr with NumericInstr with FBinOp

  case I32WrapI64 extends Instr with NumericInstr with CvtOp

  case ITruncF(intWidth: IntWidth, floatWidth: FloatWidth, signedness: Signedness)
      extends Instr
      with NumericInstr
      with CvtOp

  case I64ExtendI32(signedness: Signedness) extends Instr with NumericInstr with CvtOp
  case I64Extend32S                         extends Instr with NumericInstr with CvtOp
  case IExtend8S(intWidth: IntWidth)        extends Instr with NumericInstr with CvtOp
  case IExtend16S(intWidth: IntWidth)       extends Instr with NumericInstr with CvtOp

  case FConvertI(floatWidth: FloatWidth, intWidth: IntWidth, signedness: Signedness)
      extends Instr
      with NumericInstr
      with CvtOp

  case F32DemoteF64  extends Instr with NumericInstr with CvtOp
  case F64PromoteF32 extends Instr with NumericInstr with CvtOp

  case IReinterpretF(intWidth: IntWidth)     extends Instr with NumericInstr with CvtOp
  case FReinterpretI(floatWidth: FloatWidth) extends Instr with NumericInstr with CvtOp

  case ITruncSatF(intWidth: IntWidth, floatWidth: FloatWidth, signedness: Signedness)
      extends Instr
      with NumericInstr
      with CvtOp

  // VectorInstr
  case V128Const(value: Int128)

  // VVUnOp
  case V128Not extends Instr with VectorInstr with VVUnOp

  // VVBinOp
  case V128And    extends Instr with VectorInstr with VVBinOp
  case V128AndNot extends Instr with VectorInstr with VVBinOp
  case V128Or     extends Instr with VectorInstr with VVBinOp
  case V128XOr    extends Instr with VectorInstr with VVBinOp

  // VVTernOp
  case V128BitSelect extends Instr with VectorInstr with VVTernOp

  // VVTestOp
  case V128AnyTrue extends Instr with VectorInstr with VVTestOp

  case VI8x16Shuffle(
      laneIdx0: LaneIdx,
      laneIdx1: LaneIdx,
      laneIdx2: LaneIdx,
      laneIdx3: LaneIdx,
      laneIdx4: LaneIdx,
      laneIdx5: LaneIdx,
      laneIdx6: LaneIdx,
      laneIdx7: LaneIdx,
      laneIdx8: LaneIdx,
      laneIdx9: LaneIdx,
      laneIdx10: LaneIdx,
      laneIdx11: LaneIdx,
      laneIdx12: LaneIdx,
      laneIdx13: LaneIdx,
      laneIdx14: LaneIdx,
      laneIdx15: LaneIdx
  ) extends Instr with VectorInstr

  case VI18x16Swizzle                                              extends Instr with VectorInstr
  case VSplat(shape: Shape)                                        extends Instr with VectorInstr
  case VI8x16ExtractLane(signedness: Signedness, laneIdx: LaneIdx) extends Instr with VectorInstr
  case VI16x8ExtractLane(signedness: Signedness, laneIdx: LaneIdx) extends Instr with VectorInstr
  case VI32x4ExtractLane(laneIdx: LaneIdx)                         extends Instr with VectorInstr
  case VI64x2ExtractLane(laneIdx: LaneIdx)                         extends Instr with VectorInstr
  case VFExtractLane(shape: FShape, laneIdx: LaneIdx)              extends Instr with VectorInstr
  case VReplaceLane(shape: Shape, laneIdx: LaneIdx)                extends Instr with VectorInstr

  // VIRelOp
  case VIEq(shape: IShape) extends Instr with VectorInstr with VIRelOp
  case VINe(shape: IShape) extends Instr with VectorInstr with VIRelOp
  case VILt(shape: IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VIRelOp
  case VIGt(shape: IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VIRelOp
  case VILe(shape: IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VIRelOp
  case VIGe(shape: IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VIRelOp
  case VI64x2Lt            extends Instr with VectorInstr with VIRelOp
  case VI64x2Gt            extends Instr with VectorInstr with VIRelOp
  case VI64x2Le            extends Instr with VectorInstr with VIRelOp
  case VI64x2Ge            extends Instr with VectorInstr with VIRelOp

  // VFRelOp
  case VFEq(shape: FShape) extends Instr with VectorInstr with VFRelOp
  case VFNe(shape: FShape) extends Instr with VectorInstr with VFRelOp
  case VFLt(shape: FShape) extends Instr with VectorInstr with VFRelOp
  case VFGt(shape: FShape) extends Instr with VectorInstr with VFRelOp
  case VFLe(shape: FShape) extends Instr with VectorInstr with VFRelOp
  case VFGe(shape: FShape) extends Instr with VectorInstr with VFRelOp

  // VIUnOp
  case VIAbs(shape: IShape) extends Instr with VectorInstr with VIUnOp
  case VINeg(shape: IShape) extends Instr with VectorInstr with VIUnOp

  case VI8x16PopCnt     extends Instr with VectorInstr
  case VI16x8Q15MulrSat extends Instr with VectorInstr
  case VI32x4DotI16x8   extends Instr with VectorInstr

  // VFUnOp
  case VFAbs(shape: FShape)     extends Instr with VectorInstr with VFUnOp
  case VFNeg(shape: FShape)     extends Instr with VectorInstr with VFUnOp
  case VFSqrt(shape: FShape)    extends Instr with VectorInstr with VFUnOp
  case VFCeil(shape: FShape)    extends Instr with VectorInstr with VFUnOp
  case VFFloor(shape: FShape)   extends Instr with VectorInstr with VFUnOp
  case VFTrunc(shape: FShape)   extends Instr with VectorInstr with VFUnOp
  case VFNearest(shape: FShape) extends Instr with VectorInstr with VFUnOp

  // VITestOp
  case VIAllTrue(shape: IShape) extends Instr with VectorInstr with VITestOp

  case VIBitMask(shape: IShape) extends Instr with VectorInstr

  case VI8x16NarrowI16x8(signedness: Signedness) extends Instr with VectorInstr
  case VI16x8NarrowI32x4(signedness: Signedness) extends Instr with VectorInstr

  case VI16x8ExtendI8x16(half: Half, signedness: Signedness) extends Instr with VectorInstr with VCvtOp
  case VI32x4ExtendI16x8(half: Half, signedness: Signedness) extends Instr with VectorInstr with VCvtOp
  case VI64x2ExtendI32x4(half: Half, signedness: Signedness) extends Instr with VectorInstr with VCvtOp

  // VIShiftOp
  case VIShl(shape: IShape)                         extends Instr with VectorInstr with VIShiftOp
  case VIShr(shape: IShape, signedness: Signedness) extends Instr with VectorInstr with VIShiftOp

  // VIBinOp
  case VIAdd(shape: IShape) extends Instr with VectorInstr with VIBinOp
  case VISub(shape: IShape) extends Instr with VectorInstr with VIBinOp

  // VIMinMaxOp
  case VIMin(shape: IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VIMinMaxOp
  case VIMax(shape: IShape.I8x16.type | IShape.I16x8.type | IShape.I32x4.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VIMinMaxOp

  // VISatBinOp
  case VIAddSat(shape: IShape.I8x16.type | IShape.I16x8.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VISatBinOp
  case VISubSat(shape: IShape.I8x16.type | IShape.I16x8.type, signedness: Signedness)
      extends Instr
      with VectorInstr
      with VISatBinOp

  case VIMul(shape: IShape.I16x8.type | IShape.I32x4.type | IShape.I64x2.type) extends Instr with VectorInstr
  case VIAvgr(shape: IShape.I8x16.type | IShape.I16x8.type)                    extends Instr with VectorInstr
  case VIExtMul(shape: IShape.I16x8.type | IShape.I32x4.type | IShape.I64x2.type, half: Half, signedness: Signedness)
      extends Instr
      with VectorInstr
  case VIExtAddPairwise(shape: IShape.I16x8.type | IShape.I32x4.type, signedness: Signedness)
      extends Instr
      with VectorInstr

  // VFBinOp
  case VFAdd(shape: FShape)  extends Instr with VectorInstr with VFBinOp
  case VFSub(shape: FShape)  extends Instr with VectorInstr with VFBinOp
  case VFMul(shape: FShape)  extends Instr with VectorInstr with VFBinOp
  case VFDiv(shape: FShape)  extends Instr with VectorInstr with VFBinOp
  case VFMin(shape: FShape)  extends Instr with VectorInstr with VFBinOp
  case VFMax(shape: FShape)  extends Instr with VectorInstr with VFBinOp
  case VFPMin(shape: FShape) extends Instr with VectorInstr with VFBinOp
  case VFPMax(shape: FShape) extends Instr with VectorInstr with VFBinOp

  case VI32x4TruncSatF32x4(signedness: Signedness)     extends Instr with VectorInstr with VCvtOp
  case VI32x4TruncSatF64x2Zero(signedness: Signedness) extends Instr with VectorInstr with VCvtOp
  case VI32x4ConvertI32x4(signedness: Signedness)      extends Instr with VectorInstr with VCvtOp
  case VF32x4DemoteF64x2Zero                           extends Instr with VectorInstr with VCvtOp
  case VF64x2ConvertLowI32x4(signedness: Signedness)   extends Instr with VectorInstr with VCvtOp
  case VF64x2PromoteLowI32x4                           extends Instr with VectorInstr with VCvtOp

  // ReferenceInstr
  case RefNull(refType: RefType) extends Instr with ReferenceInstr
  case RefIsNull                 extends Instr with ReferenceInstr
  case RefFunc(idx: FuncIdx)     extends Instr with ReferenceInstr

  // ParametricInstr
  case Drop                                  extends Instr with ParametricInstr
  case Select(types: Option[Chunk[ValType]]) extends Instr with ParametricInstr

  // VariableInstr
  case LocalGet(idx: LocalIdx)   extends Instr with VariableInstr
  case LocalSet(idx: LocalIdx)   extends Instr with VariableInstr
  case LocalTee(idx: LocalIdx)   extends Instr with VariableInstr
  case GlobalGet(idx: GlobalIdx) extends Instr with VariableInstr
  case GlobalSet(idx: GlobalIdx) extends Instr with VariableInstr

  // TableInstr
  case TableGet(idx: TableIdx)                            extends Instr with TableInstr
  case TableSet(idx: TableIdx)                            extends Instr with TableInstr
  case TableSize(idx: TableIdx)                           extends Instr with TableInstr
  case TableGrow(idx: TableIdx)                           extends Instr with TableInstr
  case TableFill(idx: TableIdx)                           extends Instr with TableInstr
  case TableCopy(source: TableIdx, destination: TableIdx) extends Instr with TableInstr
  case TableInit(tableIdx: TableIdx, elemIdx: ElemIdx)    extends Instr with TableInstr
  case ElemDrop(idx: ElemIdx)                             extends Instr with TableInstr

  // MemoryInstr
  case Load(typ: NumType | VecType, memArg: MemArg)                                extends Instr with MemoryInstr
  case Store(typ: NumType | VecType, memArg: MemArg)                               extends Instr with MemoryInstr
  case Load8(typ: NumType.I32.type | NumType.I64.type, signedness: Signedness, memArg: MemArg)
      extends Instr
      with MemoryInstr
  case Load16(typ: NumType.I32.type | NumType.I64.type, signedness: Signedness, memArg: MemArg)
      extends Instr
      with MemoryInstr
  case Load32(signedness: Signedness, memArg: MemArg)                              extends Instr with MemoryInstr
  case Store8(typ: NumType.I32.type | NumType.I64.type, memArg: MemArg)            extends Instr with MemoryInstr
  case Store16(typ: NumType.I32.type | NumType.I64.type, memArg: MemArg)           extends Instr with MemoryInstr
  case Store32(memArg: MemArg)                                                     extends Instr with MemoryInstr
  case V128Load8x8(signedness: Signedness, memArg: MemArg)                         extends Instr with MemoryInstr
  case V128Load16x4(signedness: Signedness, memArg: MemArg)                        extends Instr with MemoryInstr
  case V128Load32x2(signedness: Signedness, memArg: MemArg)                        extends Instr with MemoryInstr
  case V128Load32Zero(memArg: MemArg)                                              extends Instr with MemoryInstr
  case V128Load64Zero(memArg: MemArg)                                              extends Instr with MemoryInstr
  case V128LoadSplat(loadShape: VectorLoadShape, memArg: MemArg)                   extends Instr with MemoryInstr
  case V128LoadLane(loadShape: VectorLoadShape, memArg: MemArg, laneIdx: LaneIdx)  extends Instr with MemoryInstr
  case V128StoreLane(loadShape: VectorLoadShape, memArg: MemArg, laneIdx: LaneIdx) extends Instr with MemoryInstr
  case MemorySize                                                                  extends Instr with MemoryInstr
  case MemoryGrow                                                                  extends Instr with MemoryInstr
  case MemoryFill                                                                  extends Instr with MemoryInstr
  case MemoryCopy                                                                  extends Instr with MemoryInstr
  case MemoryInit(idx: DataIdx)                                                    extends Instr with MemoryInstr
  case DataDrop(idx: DataIdx)                                                      extends Instr with MemoryInstr

  // ControlInstr
  case Nop                                                                           extends Instr with ControlInstr
  case Unreachable                                                                   extends Instr with ControlInstr
  case Block(blockType: BlockType, instructions: Chunk[Instr])                       extends Instr with ControlInstr
  case Loop(blockType: BlockType, instructions: Chunk[Instr])                        extends Instr with ControlInstr
  case If(blockType: BlockType, trueInstrs: Chunk[Instr], falseInstrs: Chunk[Instr]) extends Instr with ControlInstr
  case Br(labelIdx: LabelIdx)                                                        extends Instr with ControlInstr
  case BrIf(labelIdx: LabelIdx)                                                      extends Instr with ControlInstr
  case BrTable(labels: Chunk[LabelIdx], default: LabelIdx)                           extends Instr with ControlInstr
  case Return                                                                        extends Instr with ControlInstr
  case Call(idx: FuncIdx)                                                            extends Instr with ControlInstr
  case CallIndirect(tableIdx: TableIdx, typeIdx: TypeIdx)                            extends Instr with ControlInstr

  def foldLeftRec[S](initial: S)(f: (S, Instr) => S): S =
    this match {
      case i @ Block(_, instructions)         =>
        instructions.foldLeft(f(initial, i)) { case (s, i) => i.foldLeftRec(s)(f) }
      case i @ Loop(_, instructions)          =>
        instructions.foldLeft(f(initial, i)) { case (s, i) => i.foldLeftRec(s)(f) }
      case i @ If(_, trueInstrs, falseInstrs) =>
        falseInstrs.foldLeft(
          trueInstrs.foldLeft(f(initial, i)) { case (s, i) => i.foldLeftRec(s)(f) }
        ) { case (s, i) => i.foldLeftRec(s)(f) }
      case i: Instr                           =>
        f(initial, i)
    }

  def mapInstr(f: Instr => Instr): Instr =
    this match {
      case Block(blockType, instructions)         =>
        Block(blockType, instructions.map(_.mapInstr(f)))
      case Loop(blockType, instructions)          =>
        Loop(blockType, instructions.map(_.mapInstr(f)))
      case If(blockType, trueInstrs, falseInstrs) =>
        If(blockType, trueInstrs.map(_.mapInstr(f)), falseInstrs.map(_.mapInstr(f)))
      case i: Instr                               =>
        f(i)
    }
}

final case class Expr(instructions: Chunk[Instr]) {

  // TODO: what should these be called
  def foldLeftRec[S](initial: S)(f: (S, Instr) => S): S =
    instructions.foldLeft(initial) { case (s, i) => i.foldLeftRec(s)(f) }

  def mapInstr(f: Instr => Instr): Expr =
    this.copy(instructions = instructions.map(_.mapInstr(f)))
}

opaque type Name = String
object Name {
  def fromString(name: String): Name      = name
  def fromBytes(bytes: Chunk[Byte]): Name = new String(bytes.toArray, StandardCharsets.UTF_8)
}

extension (name: Name) {
  def toBytes: Chunk[Byte] = Chunk.fromArray(name.getBytes(StandardCharsets.UTF_8))
}
