/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.field

import com.retlinx.msg.format.charset.Charset
import com.retlinx.msg.format.length.LengthFormat
import com.retlinx.msg.types.{TYPE_DEC, TYPE_NUMSTR, Type}


abstract class FieldNum(override val T: Type,
                        override val charset: Charset,
                        override val lengthFormat: LengthFormat) extends Field(T, charset, lengthFormat) {
  require(T == TYPE_NUMSTR || T == TYPE_DEC, s"$T not allowed for this field type")

  protected var precision: Int = 0
  protected var scale: Int = 0

  override def unset: Boolean = {
    precision = 0
    scale = 0

    super.unset
  }

  def getPrecision: Int = precision

  def getScale: Int = scale

  def set(value: Int): Int

  def toInt: Int
}
