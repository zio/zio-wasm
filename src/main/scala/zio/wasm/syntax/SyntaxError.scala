package zio.wasm.syntax

import zio.parser.Parser.ParserError

enum SyntaxError {
  case InvalidLEB128
  case InvalidNumType
  case InvalidVecType
  case InvalidRefType
  case UnexpectedByte
  case InvalidCase
  case InvalidMut
  case InvalidOpcode
  case InvalidImportDesc
  case InvalidExportDesc
  case InvalidElemKind
  case InvalidElemType
  case InnerParserError(error: ParserError[SyntaxError])
  case FunctionAndCodeSectionSizeMismatch
  case RequiredSectionMissing
}
