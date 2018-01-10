/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format

import com.retlinx.msg.format.charset.{CHARSET_NONE, Charset}
import com.retlinx.msg.format.length.{LengthDelimiter, LengthFixed, LengthFormat, LengthVariable}
import com.retlinx.msg.types.Type

class Format(val name: String,
             val description: String,
             val dataType: Type,
             val dataCharset: Charset,
             val lengthFormat: LengthFormat,
             val mandatory: Boolean)


object Format {
  def apply(name: String,
            description: String,
            dataType: Type,
            dataCharset: Charset,
            length: LengthFormat,
            mask: Byte,
            mandatory: Boolean): FieldFormat =
    new FieldFormat(name, description, dataType, dataCharset, length, mask, mandatory)

  def apply(name: String,
            description: String,
            dataType: Type,
            length: Int,
            mask: Byte,
            mandatory: Boolean): FieldFormat =
    new FieldFormat(name, description, dataType, CHARSET_NONE, LengthFixed(length), mask, mandatory)

  def apply(name: String,
            description: String,
            dataType: Type,
            lengthType: Type,
            lengthSize: Int,
            min: Int,
            max: Int,
            mask: Byte,
            mandatory: Boolean): FieldFormat =
    new FieldFormat(name, description, dataType, CHARSET_NONE,
      LengthVariable(lengthType, CHARSET_NONE, lengthSize, min, max), mask, mandatory)

  def apply(name: String,
            description: String,
            dataType: Type,
            delimiter: Byte,
            min: Int,
            max: Int,
            mask: Byte,
            mandatory: Boolean): FieldFormat =
    new FieldFormat(name, description, dataType, CHARSET_NONE,
      LengthDelimiter(delimiter, min, max), mask, mandatory)

  def apply(name: String,
            description: String): MsgFormat =
    new MsgFormat(name, description)
}
