/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format.length

case class LengthDelimiter(delimiter: Byte,
                           override val min: Int,
                           override val max: Int,
                           override val scale: Int = 0) extends LengthFormat("DELIMITER", min, max, scale) {
  require(min > scale, "decimal position > min length")
  require(max > scale, "decimal position > max length")
}
