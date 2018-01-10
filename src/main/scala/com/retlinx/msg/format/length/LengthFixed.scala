/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format.length

case class LengthFixed(length: Int,
                       override val scale: Int = 0) extends LengthFormat("FIXED", length, length, scale) {
  require(length > scale, "invalid value, decimal position > min length")
  require(max > scale, "invalid value, decimal position > max length")
}