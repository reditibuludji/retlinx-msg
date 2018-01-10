/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.field

import com.retlinx.msg.format.charset.CHARSET_NONE
import com.retlinx.msg.format.length.{LengthDelimiter, LengthFixed, LengthFormat, LengthVariable}
import com.retlinx.msg.types.TYPE_DEC

import scala.annotation.tailrec


class FieldDec(override val lengthFormat: LengthFormat) extends FieldNum(TYPE_DEC, CHARSET_NONE, lengthFormat) {
  private val minInByte: Int = T.calcLengthInByte(lengthFormat.min)
  private val maxInByte: Int = T.calcLengthInByte(lengthFormat.max)

  private var varValue: Int = 0

  override protected val data: Array[Byte] = new Array(maxInByte)

  override def set: Int = {
    for (i: Int <- 0 until minInByte) data(i) = 0x00.toByte

    isset = true

    precision = lengthFormat.min
    scale = lengthFormat.scale

    varValue = lengthFormat.min
    length = minInByte

    length
  }

  override def set(value: String): Int = {
    require(value.matches("[0-9]+(\\.[0-9]+)?"), s"$this.set(value: String)value is not numeric")

    val vString: Array[String] = value.split('.')
    assert(vString.length != 2 || lengthFormat.scale != 0, s"$this.set(value: String) value has scale but property don't has scale")

    val vLenLeft: Int = vString(0).length
    val vLenRight: Int = if (vString.length == 1) 0 else vString(1).length
    assert(vLenRight >= 0 && vLenRight <= lengthFormat.scale, s"$this.set(value: String) value has scale length greater than property scale length")

    val vLenScale: Int = lengthFormat.scale

    val vLenDigit: Int = if (!lengthFormat.isInstanceOf[LengthFixed]) {
      assert(vLenLeft + vLenScale >= lengthFormat.min, s"$this.set(value: String) length less than minimum length")

      assert(vLenLeft + vLenScale <= lengthFormat.max, s"$this.set(value: String) length greater than maximum length")

      vLenLeft
    }
    else {
      assert(vLenLeft + vLenScale <= lengthFormat.max, s"$this.set(value: String) length greater than maximum length")
      assert(vLenLeft <= lengthFormat.max - vLenScale, s"$this.set(value: String) scale has invalid length")

      if (lengthFormat.max - lengthFormat.scale > vLenLeft) lengthFormat.max - lengthFormat.scale else vLenLeft
    }

    assert(T.calcLengthInByte(vLenDigit + vLenScale) <= data.length, "set value with overflow copy length")

    val vValue: String = {
      val vLeft: Array[Byte] = Array.fill[Byte](vLenDigit)('0')
      Array.copy(vString(0).getBytes, 0, vLeft, vLenDigit - vLenLeft, vLenLeft)
      if (vLenScale == 0) {
        val s: String = new String(vLeft)
        if (s.length % 2 == 0) s else "0" + s
      }
      else {
        val vRight: Array[Byte] = Array.fill[Byte](lengthFormat.scale)('0')
        if (vLenRight != 0) {
          Array.copy(vString(1).getBytes, 0, vRight, 0, vLenRight)
        }
        val s: String = new String(vLeft ++ vRight)
        if (s.length % 2 == 0) s else "0" + s
      }
    }
    var i: Int = 0
    vValue.replaceAll("[^0-9]", "").sliding(2, 2).toArray.foreach(x => {
      data(i) = Integer.parseInt(x, 16).toByte
      i = i + 1
    })

    isset = true
    precision = vLenLeft + vLenRight
    scale = vLenRight

    varValue = vLenDigit + vLenScale
    length = T.calcLengthInByte(varValue)

    length
  }

  override def toStr(formatted: Char = ' '): String = {

    def formatFixed(s: String): String = {
      val vDigits = lengthFormat.max - lengthFormat.scale
      val vScale = lengthFormat.scale
      if (vScale == 0) {
        s.takeRight(precision)
      }
      else {
        val vLeft = precision - scale
        if (scale == 0) {
          s.slice(vDigits - vLeft, vDigits)
        }
        else
          s.slice(vDigits - vLeft, vDigits) + '.' + s.takeRight(lengthFormat.scale).take(scale)
      }
    }

    def formatVariable(s: String): String = {
      if (lengthFormat.scale == 0) {
        s.take(precision)
      }
      else {
        val vLeft = precision - scale
        if (scale == 0)
          s.take(vLeft)
        else
          s.take(vLeft) + '.' + s.takeRight(lengthFormat.scale).take(scale)
      }
    }

    assert(isset, s"$this.toStr field not set")


    val s: String = data.take(length).map("%02X".format(_)).mkString.takeRight(varValue)

    if (formatted != 'd')
      s
    else {
      lengthFormat match {
        case f: LengthFixed => formatFixed(s)
        case v: LengthVariable => formatVariable(s)
        case d: LengthDelimiter => formatVariable(s)
        case _: LengthFormat => throw new RuntimeException(s"$this.toStr unsupported Length Property")
      }
    }
  }

  override def unpack(value: Array[Byte], pos: Int): Int =lengthFormat match {
    case f: LengthFixed =>
      assert(pos + T.calcLengthInByte(f.max) <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) overflow buffer")
      assert(T.validate(value, pos, T.calcLengthInByte(f.max), charset), s"$this.unpack(value: Array[Byte], pos: Int) value has invalid char")

      Array.copy(value, pos, data, 0, T.calcLengthInByte(f.max))

      isset = true
      length = T.calcLengthInByte(f.max)
      varValue = f.max

      precision = f.max
      scale = f.scale

      pos + length

    case v: LengthVariable =>
      assert(pos + v.dataLength <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) overflow buffer for header")

      val newPos = varLength.unpack(value, pos)
      val newLength = getVarLength
      val newCopyLength = T.calcLengthInByte(newLength)

      assert(pos + newCopyLength <= value.length, s"$this.unpack(value: Array[Byte], pos: Int) overflow buffer")
      assert(newLength >= v.min, s"$this.unpack(value: Array[Byte], pos: Int) length less than mininum")
      assert(newLength <= v.max, s"$this.unpack(value: Array[Byte], pos: Int) length greater than maximum")
      assert(T.validate(value, newPos, newCopyLength, charset), s"$this.unpack(value: Array[Byte], pos: Int) invalid content")

      Array.copy(value, newPos, data, 0, newCopyLength)

      isset = true

      precision = newLength
      scale = v.scale

      length = newCopyLength
      varValue = newLength

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

      precision = newLength * 2
      scale = d.scale

      varValue = newLength * 2
      length = newLength

      pos + length + 1

    case _ => throw new RuntimeException("invalid length property when unpacking")
  }

  override def pack: Array[Byte] = lengthFormat match {
    case f: LengthFixed => getBytes
    case d: LengthDelimiter => getBytes ++ Array(d.delimiter)
    case v: LengthVariable =>
      setVarLength(varValue)
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
      setVarLength(varValue)
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
    str.append("[FIELD DEC:")
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
