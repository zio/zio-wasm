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
  case InvalidOpcode(prefix: Option[Byte], code: Int)
  case InvalidImportDesc
  case InvalidExportDesc
  case InvalidElemKind
  case InnerParserError(error: ParserError[SyntaxError])
  case FunctionAndCodeSectionSizeMismatch

  case InvalidDigit
  case InvalidComment
  case UnexpectedKeyword(error: String)
  case Int32TooLarge
  case InvalidSign
  case InvalidGlobalType
  case InvalidId
  case InvalidInstruction

  override def toString: String = this match {
    case InvalidLEB128                      => "Invalid LEB128"
    case InvalidNumType                     => "Invalid num type"
    case InvalidVecType                     => "Invalid vec type"
    case InvalidRefType                     => "Invalid ref type"
    case UnexpectedByte                     => "Unexpected byte"
    case InvalidCase                        => "Invalid case"
    case InvalidMut                         => "Invalid mut"
    case InvalidOpcode(prefix, code)        =>
      s"Invalid opcode: ${prefix
          .map(_.toInt.toHexString)}, 0x${if (prefix.isEmpty) (code & 0xff).toHexString else code.toHexString}"
    case InvalidImportDesc                  => "Invalid import desc"
    case InvalidExportDesc                  => "Invalid export desc"
    case InvalidElemKind                    => "Invalid elem kind"
    case InnerParserError(error)            => error.toString
    case FunctionAndCodeSectionSizeMismatch => "Function and code section size mismatch"
    case InvalidDigit                       => "Invalid digit"
    case InvalidComment                     => "Invalid comment"
    case UnexpectedKeyword(error)           => s"Unexpected keyword ($error)"
    case Int32TooLarge                      => "Int32 too large"
    case InvalidSign                        => "Invalid sign"
    case InvalidGlobalType                  => "Invalid global type"
    case InvalidId                          => "Invalid id"
    case InvalidInstruction                 => "Invalid instruction"
  }
}
