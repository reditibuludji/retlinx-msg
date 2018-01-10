/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.field

import com.retlinx.msg.format.charset.CHARSET_NONE
import com.retlinx.msg.format.length.{LengthDelimiter, LengthFixed, LengthFormat, LengthVariable}
import com.retlinx.msg.types.TYPE_BYTE

import scala.annotation.tailrec


class FieldByte(override val lengthFormat: LengthFormat) extends Field(TYPE_BYTE, CHARSET_NONE, lengthFormat) {

  T.required(charset, lengthFormat)

  override protected val data: Array[Byte] = new Array[Byte](lengthFormat.max)

  override def set: Int = {
    for (i: Int <- 0 until lengthFormat.min) data(i) = 0x00.toByte

    isset = true
    length = lengthFormat.min

    length
  }

  override def set(value: String): Int = {
    val a: Array[Byte] = if (value.startsWith("x|")) {
      val s: String = value.drop(2)
      assert(s.length % 2 == 0, s"$this.set(value: String) value invalid length")
      assert(s.matches("[^0-9A-Fa-f]"), s"$this.set(value: String) value with invalid content")
      s.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
    else if (value.startsWith("b|")) {
      val s: String = value.drop(2)

      assert(s.length % 8 == 0, s"$this.set(value: String) invalid length for binary")
      assert(s.matches("[01]+"), s"$this.set(value: String) invalid content for binary")
      s.replaceAll("[^0-1]", "").sliding(8, 8).toArray.map(Integer.parseInt(_, 2).toByte)
    }
    else
      value.getBytes

    set(a)
  }

  override def toStr(formatted: Char): String = {
    formatted match {
      case 'x' | 'X' => "x|" + data.take(length).map("%02x".format(_)).mkString
      case 'b' | 'B' => "b|" + data.take(length).map(e => {
        "%8s".format(Integer.toBinaryString(e.toInt & 0xff)).replace(' ', '0')
      }).mkString
      case _ => new String(data.take(length))
    }
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
        if (found) {
          if (l == 0) 0 else l - 1
        }
        else {
          if (p > value.length) -1 else find(p + 1, l + 1, value(p) == d.delimiter)
        }
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

  def set(value: Array[Byte]): Int = {
    if (lengthFormat.min != lengthFormat.max ) {
      assert(value.length >= lengthFormat.min, s"this.set(value: String) length less than minimum length")
      assert(value.length <= lengthFormat.max, s"this.set(value: String) length greater than maximum length")
    }
    else {
      assert(value.length == lengthFormat.max, s"this.set(value: String) length is not equal with fixed length")
    }

    for (i: Int <- value.indices) data(i) = value(i)

    isset = true
    length = value.length

    length
  }

  override def toString: String = {
    val str: StringBuilder = StringBuilder.newBuilder
    str.append("[FIELD BYTE:")
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
