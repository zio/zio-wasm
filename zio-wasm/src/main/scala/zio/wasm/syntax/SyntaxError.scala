package zio.wasm.syntax

import zio.parser.Parser.ParserError

enum SyntaxError {
  case InvalidLEB128
  case InvalidNumType(byte: Byte)
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
  case EmptyVector
  case OldComponentVersion(byte: Byte)
  case NewComponentVersion(byte: Byte)

  override def toString: String = this match {
    case InvalidLEB128                      => "Invalid LEB128"
    case InvalidNumType(byte)               => s"Invalid num type ($byte)"
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
    case EmptyVector                        => "Unexpected empty vector"
    case OldComponentVersion(version)       => s"Old component version: $version"
    case NewComponentVersion(version)       => s"New component version: $version"
  }
}
