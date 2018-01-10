/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.field

import com.retlinx.msg.format.charset.{CHARSET_ASCII, CHARSET_EBCDIC, CHARSET_NONE, Charset}
import com.retlinx.msg.format.length.{LengthDelimiter, LengthFixed, LengthFormat, LengthVariable}
import com.retlinx.msg.types.TYPE_NUMSTR

import scala.annotation.tailrec

class FieldNumStr(override val charset: Charset,
                  override val lengthFormat: LengthFormat) extends FieldNum(TYPE_NUMSTR, charset, lengthFormat) {

  T.required(charset, lengthFormat)

  override protected val data: Array[Byte] = new Array[Byte](lengthFormat.max)

  override def set: Int = {
    charset match {
      case CHARSET_NONE   => for (i: Int <- 0 until lengthFormat.min) data(i) = '0'
      case CHARSET_ASCII  => for (i: Int <- 0 until lengthFormat.min) data(i) = 0x30.toByte
      case CHARSET_EBCDIC => for (i: Int <- 0 until lengthFormat.min) data(i) = 0xf0.toByte
    }

    isset = true
    length = lengthFormat.min

    precision = lengthFormat.min
    scale = lengthFormat.scale

    length
  }

  override def set(value: String): Int = {
    require(value.matches("[0-9]+(\\.[0-9]+)?"), s"$this.set(value: String) set value with value is not numeric")

    val vString: Array[String] = value.split('.')
    assert(vString.length != 2 || lengthFormat.scale != 0, s"$this.set(value: String) value has scale but property don't has scale")

    val vLenLeft: Int = vString(0).length
    val vLenRight: Int = if (vString.length == 1) 0 else vString(1).length
    assert(vLenRight >= 0 && vLenRight <= lengthFormat.scale, s"$this.set(value: String) value has scale greater than property scale")

    val vLenScale: Int = lengthFormat.scale

    val vLenDigit: Int = if (!lengthFormat.isInstanceOf[LengthFixed]) {
      assert(vLenLeft + vLenScale >= lengthFormat.min, s"$this.set(value: String) length less than minimum length")
      assert(vLenLeft + vLenScale <= lengthFormat.max, s"$this.set(value: String) length greater than maximum length")

      vLenLeft
    }
    else {
      assert(vLenLeft + vLenScale <= lengthFormat.max, s"$this.set(value: String) length greater than maximum length")
      assert(vLenLeft <= lengthFormat.max - vLenScale, s"$this.set(value: String) scale has invalid length")

      if (lengthFormat.max - lengthFormat.scale > vLenLeft)
        lengthFormat.max - lengthFormat.scale
      else
        vLenLeft
    }

    assert(vLenDigit + vLenScale <= data.length, "set value with overflow copy length")

    val vValue: Array[Byte] = {
      val vLeft: Array[Byte] = Array.fill[Byte](vLenDigit)('0')
      Array.copy(vString(0).getBytes, 0, vLeft, vLenDigit - vLenLeft, vLenLeft)
      if (vLenScale == 0) {
        vLeft
      }
      else {
        val vRight: Array[Byte] = Array.fill[Byte](lengthFormat.scale)('0')
        if (vLenRight != 0) {
          Array.copy(vString(1).getBytes, 0, vRight, 0, vLenRight)
        }
        vLeft ++ vRight
      }
    }

    if (charset == CHARSET_NONE || charset == CHARSET_ASCII) {
      for (i: Int <- vValue.indices) data(i) = vValue(i)
    }
    else {
      for (i: Int <- vValue.indices) data(i) = CHARSET_NONE.a2e(vValue(i) & 0xff)
    }

    precision = vLenLeft + vLenRight
    scale = vLenRight
    isset = true
    length = vLenDigit + vLenScale
    length
  }

  override def toStr(formatted: Char = ' '): String = {

    def getBytes(length: Int): Array[Byte] = {
      charset match {
        case CHARSET_NONE   => data.take(length)
        case CHARSET_ASCII  => data.take(length)
        case CHARSET_EBCDIC => data.take(length).map(e  => CHARSET_NONE.e2a(e & 0xff))
      }
    }

    def formatFixed(b: Array[Byte]): String = {
      val vDigits = lengthFormat.max - lengthFormat.scale
      val vScale = lengthFormat.scale
      if (vScale == 0) {
        new String(b.drop(vDigits - precision))
      }
      else {
        val vLeft = precision - scale
        if (scale == 0)
          new String(b.slice(vDigits - vLeft, vDigits))
        else
          new String(b.slice(vDigits - vLeft, vDigits) ++ Array('.'.toByte) ++ b.takeRight(lengthFormat.scale).take(scale))
      }
    }

    def formatVariable(b: Array[Byte]): String = {
      if (lengthFormat.scale == 0) {
        new String(b.takeRight(precision))
      }
      else {
        val vLeft = precision - scale
        if (scale == 0)
          new String(b.take(vLeft))
        else
          new String(b.take(vLeft) ++ Array('.'.toByte) ++ b.takeRight(lengthFormat.scale).take(scale))
      }
    }

    assert(isset, s"$this.toStr field not set")

    val b: Array[Byte] = getBytes(length)
    if (formatted != 'd')
      new String(b)
    else {
      lengthFormat match {
        case _: LengthFixed => formatFixed(b)
        case _: LengthVariable => formatVariable(b)
        case _: LengthDelimiter => formatVariable(b)
        case _: LengthFormat => throw new RuntimeException("toStr with unsupport Length Property")
      }
    }
  }

  override def unpack(value: Array[Byte], pos: Int): Int = lengthFormat match {
    case f: LengthFixed =>
      assert(pos + f.max <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) overflow buffer")
      assert(T.validate(value, pos, f.max, charset), s"$this.unpack(value: Array[Byte], pos: Int) value has invalid char")

      Array.copy(value, pos, data, 0, f.max)

      isset = true
      length = f.max

      precision = f.max
      scale = f.scale

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

      precision = newLength
      scale = v.scale

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

      precision = newLength
      scale = d.scale

      pos + length + 1

    case _ => throw new RuntimeException("invalid length property when unpacking")
  }

  override def pack: Array[Byte] = lengthFormat match {
    case _: LengthFixed => getBytes
    case d: LengthDelimiter => getBytes ++ Array(d.delimiter)
    case _: LengthVariable =>
      setVarLength(length)
      varLength.getBytes ++ getBytes
    case _ => throw new RuntimeException(s"$this.pack length type not support")
  }

  override def pack(buffer: Array[Byte], pos: Int): Int = lengthFormat match {
    case _: LengthFixed =>
      assert(pos + length <= buffer.length, s"$this.pack(buffer: Array[Byte]) overflow buffer length")
      Array.copy(data, 0, buffer, pos, length)
      pos + length

    case d: LengthDelimiter =>
      assert(pos + length + 1 <= buffer.length, s"$this.pack(buffer: Array[Byte]) overflow buffer length")
      Array.copy(data, 0, buffer, pos, length)
      buffer(pos + length) = d.delimiter
      pos + length + 1

    case _: LengthVariable =>
      setVarLength(length)
      val newPos: Int = varLength.pack(buffer, pos)
      assert(newPos + length <= buffer.length, s"$this.pack(buffer: Array[Byte], pos: Int) overflow buffer length")
      Array.copy(data, 0, buffer, newPos, length)
      newPos + length

    case _ => throw new RuntimeException(s"$this.pack(buffer: Array[Byte], pos: Int) length type not support")
  }

  override def set(value: Int): Int = {
    val s: String = value.toString
    set(s)
  }

  override def toInt: Int = {
    assert(scale == 0, s"$this.toInt but value is not integer, has decimal point")
    this.toStr().toInt
  }

  override def toString: String = {
    val str: StringBuilder = StringBuilder.newBuilder
    str.append("[FIELD NUMSTR:")
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
    str.append(',')
    str.append(lengthFormat.scale)
    str.append(")]")
    str.toString
  }
}
