/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format

import com.retlinx.msg.format.charset.CHARSET_NONE
import com.retlinx.msg.format.length.{LengthNone, LengthVariable}
import com.retlinx.msg.types.TYPE_NONE

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class MsgFormat(override val name: String,
                override val description: String) extends Format(name, description, TYPE_NONE, CHARSET_NONE, LengthNone(), true) {

  dataType.required(dataCharset, lengthFormat)

  val fields: ArrayBuffer[Format] = ArrayBuffer[Format]()
  val fieldsByName: mutable.Map[String, Int] = mutable.Map[String, Int]()

  def add(field: Format): Unit  = {
    if (fieldsByName.get(field.name).isDefined) throw new RuntimeException("duplicate field by name")

    fields += field
    fieldsByName(field.name) = fields.length - 1
  }

  def size: Int = fields.length

  def minLength: Int =
    fields.map(f => if (f.mandatory) {
      f.lengthFormat match {
        case v: LengthVariable => v.dataType.calcLengthInByte(v.dataLength)
        case _ => return 0
      }
      + f.dataType.calcLengthInByte(f.lengthFormat.min)
    }
    else
      0).sum

  def maxLength: Int =
    fields.map(f => {
      f.lengthFormat match {
        case v: LengthVariable => v.dataType.calcLengthInByte(v.dataLength)
        case _ => return 0
      }
      + f.dataType.calcLengthInByte(f.lengthFormat.max)
    }).sum


  def getFields: Array[Format] = fields.toArray
  def getField(name: String): Option[Format] = {
    val i: Option[Int] = fieldsByName.get(name)
    if (i.isDefined) Some(fields(i.get)) else None
  }
}
