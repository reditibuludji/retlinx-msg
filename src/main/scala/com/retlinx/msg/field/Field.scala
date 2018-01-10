/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.field

import com.retlinx.msg.Ref
import com.retlinx.msg.format.charset.{CHARSET_NONE, Charset}
import com.retlinx.msg.format.length.{LengthFixed, LengthFormat, LengthVariable}
import com.retlinx.msg.types.{TYPE_BYTE, TYPE_DEC, TYPE_NUMSTR, TYPE_STR, Type}

abstract class Field(override val T: Type,
                     val charset: Charset,
                     val lengthFormat: LengthFormat) extends Ref(T) {

  protected val data: Array[Byte]

  protected[this] lazy val varLength: Field = lengthFormat match {
    case l: LengthVariable => Field(l.dataType, l.dataCharset, LengthFixed(l.dataLength))
    case _ => null
  }

  protected[this] def setVarLength(len: Int): Int = {
    if (varLength != null) {
      varLength match {
        case b: FieldByte =>
          val aLength: Array[Byte] = Array(((len >> 8) & 0xff).toByte, (len & 0xff).toByte)
          b.asInstanceOf[FieldByte].set(aLength)
        case f: FieldNum =>
          f.set(len)
        case _ =>
          throw new RuntimeException(s"$this.setVarField length no match field type")
      }
    }
    else
      throw new RuntimeException(s"$this.setVarField length property is not LengthVariable")
  }

  protected[this] def getVarLength: Int = {
    if (varLength != null) {
      varLength match {
        case b: FieldByte =>
          val aLength: Array[Byte] = b.getBytes
          ((aLength(0) << 8) & 0xff00) + (aLength(1) & 0xff)
        case f: FieldNum =>
          f.toInt
        case _ =>
          throw new RuntimeException(s"$this.getVarField length no match field type")
      }
    }
    else
      throw new RuntimeException(s"$this.getVarField length property is not LengthVariable")
  }

  def unset: Boolean = {
    isset = false
    isset
  }

  def set: Int

  def set(value: String): Int

  def toStr(formatted: Char = ' '): String

  def getBytes: Array[Byte] = {
    assert(isset, "field not set")
    assert(length != 0, "length in byte = 0")

    data.take(length)
  }

  def pack: Array[Byte]
}


object Field {
  def apply(t: Type, charset: Charset, lengthFormat: LengthFormat): Field = t match {
    case TYPE_BYTE => new FieldByte(lengthFormat)
    case TYPE_DEC => new FieldDec(lengthFormat)
    case TYPE_NUMSTR => new FieldNumStr(charset, lengthFormat)
    case TYPE_STR => new FieldStr(charset, lengthFormat)
    case _ => throw new RuntimeException(s"TYPE: $t is not support")
  }

  def apply(t: Type, lengthFormat: LengthFormat): Field = apply(t, CHARSET_NONE, lengthFormat)
}
