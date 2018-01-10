/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.types

import com.retlinx.msg.format.charset.{CHARSET_NONE, Charset}
import com.retlinx.msg.format.length.LengthFormat

import scala.annotation.tailrec

case object TYPE_DEC extends Type {
  override def calcLengthInByte(length: Int): Int = if (length % 2 == 0) length / 2 else (length + 1) / 2

  override def required(charset: Charset, lengthFormat: LengthFormat): Unit = {
    require(charset == CHARSET_NONE, "invalid charset, must CHARSET_NONE")

    if (lengthFormat.max == lengthFormat.min) {
      assert(lengthFormat.max > lengthFormat.scale, "invalid length format with scale value")
    }
    else {
      assert(lengthFormat.min > lengthFormat.scale, "invalid length format with scale (min)")
      assert(lengthFormat.max> lengthFormat.scale, "invalid length format with scale value (max)")
    }
  }

  override def validate(data: Array[Byte], pos: Int, length: Int, charset: Charset): Boolean = {
    assert(pos + length <= data.length, "validate content failed, overflow length checking")

    def isValid(b: Byte): Boolean = {
      val lo: Int = b & 0x0f
      val hi: Int = (b >> 4).toByte & 0x0f

      if ((lo >= 0 && lo <= 9) && (hi >= 0 && hi <= 9)) true else false
    }

    @tailrec
    def check(f: Byte => Boolean, p: Int, r: Boolean): Boolean = {
      if (r) {
        if (p + 1 == length) f(data(pos + p)) else check(f, p + 1, f(data(pos + p + 1)))
      }
      else
        false
    }

    check(isValid, 0, isValid(data(pos)))
  }

  override def validate(data: Array[Byte], charset: Charset): Boolean = validate(data, 0, data.length, charset)
}
