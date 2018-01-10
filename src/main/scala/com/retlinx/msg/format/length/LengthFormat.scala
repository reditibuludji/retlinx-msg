/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format.length

abstract class LengthFormat(val name: String, val min: Int, val max: Int, val scale: Int = 0) {
  require(min <= max, "invalid value, min > max")

  override def toString: String = name
}




