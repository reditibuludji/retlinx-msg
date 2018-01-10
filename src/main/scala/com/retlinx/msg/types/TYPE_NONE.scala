/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.types

import com.retlinx.msg.format.charset.{CHARSET_NONE, Charset}
import com.retlinx.msg.format.length.{LengthFormat, LengthNone}

case object TYPE_NONE extends Type {
  override def calcLengthInByte(length: Int): Int = 0

  override def required(charset: Charset, lengthFormat: LengthFormat): Unit = {
    require(charset == CHARSET_NONE, "invalid charset, must CHARSET_NONE")
    require(lengthFormat.isInstanceOf[LengthNone], "invalid length property, must LengthNone")
  }

  override def validate(data: Array[Byte], pos: Int, length: Int, charset: Charset): Boolean = false
  override def validate(data: Array[Byte], charset: Charset): Boolean = false
}
