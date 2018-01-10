/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg

import com.retlinx.msg.types.Type

abstract class Ref(val T: Type) {
  protected var isset: Boolean = false
  protected var length: Int = 0

  def isSet: Boolean = isset
  def getLength: Int = length

  def unpack(fromBuffer: Array[Byte], fromPos: Int): Int
  def pack(toBuffer: Array[Byte], toPos: Int): Int
}
