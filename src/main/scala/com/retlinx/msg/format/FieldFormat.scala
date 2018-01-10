/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format

import com.retlinx.msg.format.charset.Charset
import com.retlinx.msg.format.length.LengthFormat
import com.retlinx.msg.types.Type

class FieldFormat(override val name: String,
                  override val description: String,
                  override val dataType:Type,
                  override val dataCharset: Charset,
                  override val lengthFormat: LengthFormat,
                  val mask: Byte,
                  override val mandatory: Boolean) extends Format(name, description, dataType, dataCharset, lengthFormat, mandatory) {
  dataType.required(dataCharset, lengthFormat)
}