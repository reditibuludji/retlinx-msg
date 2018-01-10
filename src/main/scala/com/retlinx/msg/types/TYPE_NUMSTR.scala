/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.types

import com.retlinx.msg.format.charset.{CHARSET_ASCII, CHARSET_EBCDIC, CHARSET_NONE, Charset}
import com.retlinx.msg.format.length.LengthFormat

import scala.annotation.tailrec


case object TYPE_NUMSTR extends Type {
  override def calcLengthInByte(length: Int): Int = length

  override def required(charset: Charset, lengthFormat: LengthFormat): Unit = {
    if (lengthFormat.max == lengthFormat.min) {
      assert(lengthFormat.max > lengthFormat.scale, "invalid length property with scale value")
    }
    else {
      assert(lengthFormat.min > lengthFormat.scale, "invalid length property with scale (min)")
      assert(lengthFormat.max > lengthFormat.scale, "invalid length property with scale value (max)")
    }
  }

  override def validate(data: Array[Byte], pos: Int, length: Int, charset: Charset): Boolean = {
    assert(pos + length <= data.length, "validate content failed, overflow length checking")

    def isNone(b: Byte): Boolean =
      if (b < '0' || b > '9') false else true

    def isAscii(b: Byte): Boolean =
      if (b < 0x30.toByte || b > 0x39.toByte) false else true

    def isEbcdic(b: Byte): Boolean =
      if (b < 0xf0.toByte || b > 0xf9.toByte) false else true

    @tailrec
    def check(f: Byte => Boolean, p: Int, r: Boolean): Boolean = {
      if (r) {
        if (p + 1 == length) f(data(pos + p)) else check(f, p + 1, f(data(pos + p + 1)))
      }
      else
        false
    }

    charset match {
      case CHARSET_NONE => check(isNone, 0, isNone(data(pos)))
      case CHARSET_ASCII => check(isAscii, 0, isAscii(data(pos)))
      case CHARSET_EBCDIC => check(isEbcdic, 0, isEbcdic(data(pos)))
      case _ => throw new RuntimeException("unknown charset")
    }
  }

  override def validate(data: Array[Byte], charset: Charset): Boolean = validate(data, 0, data.length, charset)
}