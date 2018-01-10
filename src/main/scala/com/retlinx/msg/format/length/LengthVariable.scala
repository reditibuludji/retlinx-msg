/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format.length

import com.retlinx.msg.format.charset.Charset
import com.retlinx.msg.types.{TYPE_BYTE, TYPE_DEC, TYPE_NUMSTR, Type}

case class LengthVariable(dataType: Type,
                          dataCharset: Charset,
                          dataLength: Int,
                          override val min: Int,
                          override val max: Int,
                          override val scale: Int = 0) extends LengthFormat("LXVAR", min, max, scale) {

  require(dataType == TYPE_NUMSTR || dataType == TYPE_DEC  || dataType == TYPE_BYTE,
    "unsupported data type for length variable")

  require(min > scale, "decimal position > min length")
  require(max > scale, "decimal position > max length")
}
