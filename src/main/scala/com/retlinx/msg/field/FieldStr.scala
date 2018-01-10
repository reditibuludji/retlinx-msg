/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.field

import com.retlinx.msg.format.charset.{CHARSET_ASCII, CHARSET_EBCDIC, CHARSET_NONE, Charset}
import com.retlinx.msg.format.length.{LengthDelimiter, LengthFixed, LengthFormat, LengthVariable}
import com.retlinx.msg.types.TYPE_STR

import scala.annotation.tailrec


class FieldStr(override val charset: Charset,
          override val lengthFormat: LengthFormat) extends Field(TYPE_STR, charset, lengthFormat) {

  T.required(charset, lengthFormat)

  override protected val data: Array[Byte] = new Array[Byte](lengthFormat.max)

  override def set: Int = {
    charset match {
      case CHARSET_NONE   => for (i: Int <- 0 until lengthFormat.min) data(i) = ' '
      case CHARSET_ASCII  => for (i: Int <- 0 until lengthFormat.min) data(i) = ' ' //0x30.toByte
      case CHARSET_EBCDIC => for (i: Int <- 0 until lengthFormat.min) data(i) = 0x40.toByte
    }

    isset = true
    length = lengthFormat.min

    length
  }

  override def set(value: String): Int = {
    if (lengthFormat.min != lengthFormat.max ) {
      assert(value.length >= lengthFormat.min, s"this.set(value: String) length less than minimum length")
      assert(value.length <= lengthFormat.max, s"this.set(value: String) length greater than maximum length")
    }
    else {
      assert(value.length == lengthFormat.max, s"this.set(value: String) length is not equal with fixed length")
    }

    if (charset == CHARSET_NONE || charset == CHARSET_ASCII) {
      for (i: Int <- value.indices) data(i) = value(i).toByte
    }
    else {
      for (i: Int <- value.indices) data(i) = CHARSET_NONE.a2e(value(i).toByte & 0xff)
    }

    isset = true
    length = value.length

    length
  }

  override def toStr(formatted: Char): String = charset match {
    case CHARSET_NONE   => new String(data.take(length))
    case CHARSET_ASCII  => new String(data.take(length))
    case CHARSET_EBCDIC => new String(data.take(length).map(e  => CHARSET_NONE.e2a(e & 0xff)))
  }

  override def unpack(value: Array[Byte], pos: Int): Int = lengthFormat match {
    case f: LengthFixed =>
      assert(pos + f.max <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) overflow buffer")
      assert(T.validate(value, pos, f.max, charset), s"$this.unpack(value: Array[Byte], pos: Int) value has invalid char")

      Array.copy(value, pos, data, 0, f.max)

      isset = true
      length = f.max

      pos + length

    case v: LengthVariable =>
      assert(pos + v.dataLength <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) overflow buffer for header")

      val newPos = varLength.unpack(value, pos)
      val newLength = getVarLength

      assert(pos + newLength <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) overflow buffer")
      assert(newLength >= v.min, s"$this.unpack(value: Array[Byte], pos: Int) length less than mininum")
      assert(newLength <= v.max, s"$this.unpack(value: Array[Byte], pos: Int) length greater than maximum")
      assert(T.validate(value, newPos, newLength, charset), s"$this.unpack(value: Array[Byte], pos: Int) invalid content")

      Array.copy(value, newPos, data, 0, newLength)

      isset = true
      length = newLength

      newPos + length

    case d: LengthDelimiter =>
      @tailrec
      def find(p: Int, l: Int, found: Boolean): Int = {
        if (found)
          if (l == 0) 0 else l - 1
        else
          if (p > value.length) -1 else find(p + 1, l + 1, value(p) == d.delimiter)
      }

      val newLength = find(pos, 0, value(pos) == d.delimiter)
      assert(newLength >= 0, s"$this.unpack(value: Array[Byte], pos: Int) can't find delimiter")

      assert(pos + newLength <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) with overflow length")
      assert(newLength >= d.min, s"$this.unpack(value: Array[Byte], pos: Int) length less than mininum")
      assert(newLength <= d.max, s"$this.unpack(value: Array[Byte], pos: Int) length greater than maximum")
      assert(T.validate(value, pos, newLength, charset), s"$this.unpack(value: Array[Byte], pos: Int) invalid content")

      Array.copy(value, pos, data, 0, newLength)

      isset = true
      length = newLength

      pos + length + 1

    case _ => throw new RuntimeException("invalid length property when unpacking")
  }


  override def pack: Array[Byte] = lengthFormat match {
    case f: LengthFixed => getBytes
    case d: LengthDelimiter => getBytes ++ Array(d.delimiter)
    case v: LengthVariable =>
      setVarLength(length)
      varLength.getBytes ++ getBytes
    case _ => throw new RuntimeException(s"$this.pack length type not support")
  }

  override def pack(buffer: Array[Byte], pos: Int): Int = lengthFormat match {
    case f: LengthFixed =>
      assert(pos + length <= buffer.length, s"$this.pack(buffer: Array[Byte]) overflow buffer length")
      Array.copy(data, 0, buffer, pos, length)
      pos + length

    case d: LengthDelimiter =>
      assert(pos + length + 1 <= buffer.length, s"$this.pack(buffer: Array[Byte]) overflow buffer length")
      Array.copy(data, 0, buffer, pos, length)
      buffer(pos + length) = d.delimiter
      pos + length + 1

    case v: LengthVariable =>
      setVarLength(length)
      val newPos: Int = varLength.pack(buffer, pos)
      assert(newPos + length <= buffer.length, s"$this.pack(buffer: Array[Byte], pos: Int) overflow buffer length")
      Array.copy(data, 0, buffer, newPos, length)
      newPos + length

    case _ => throw new RuntimeException(s"$this.pack(buffer: Array[Byte], pos: Int) length type not support")
  }

  override def toString: String = {
    val str: StringBuilder = StringBuilder.newBuilder
    str.append("[FIELD STR:")
    if (charset != CHARSET_NONE) {
      str.append(charset)
      str.append(':')
    }
    str.append(lengthFormat)
    str.append('(')
    str.append(lengthFormat.min)
    if (!lengthFormat.isInstanceOf[LengthFixed]) {
      str.append('-')
      str.append(lengthFormat.max)
    }
    str.append(")]")
    str.toString
  }
}
