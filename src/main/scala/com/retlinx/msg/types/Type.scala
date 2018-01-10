/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.types

import com.retlinx.msg.format.charset.Charset
import com.retlinx.msg.format.length.LengthFormat

trait Type {
  def calcLengthInByte(length: Int): Int

  def required(charset: Charset, lengthFormat: LengthFormat): Unit

  def validate(data: Array[Byte], pos: Int, length: Int, charset: Charset): Boolean
  def validate(data: Array[Byte], charset: Charset): Boolean
}
