/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg

import com.retlinx.msg.field.Field
import com.retlinx.msg.format.{FieldFormat, MsgFormat}
import com.retlinx.msg.types.TYPE_NONE

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


abstract class Msg(protected val format: MsgFormat, val usingBuffer: Boolean = false) extends Ref(TYPE_NONE) {

  val fields: ArrayBuffer[Ref] = new ArrayBuffer[Ref](format.size)

  val data: Array[Byte]

  format.fields.foreach(f => {
    fields += Field(f.dataType, f.dataCharset, f.lengthFormat)
  })


  def unpack(buffer: Array[Byte], pos: Int = 0): Int = {

    @tailrec
    def _unpack(i: Int, buf: Array[Byte], p: Int): Int = {

      if (i >= fields.length) p
      else {
        fields(i) match {
          case f: Field =>
            if (format.fields(i).mandatory) {
              _unpack(i + 1, buf, f.unpack(buf, p))
            }
            else
              throw new RuntimeException(s"$this.unpack(fromBuffer: Array[Byte], pos: Int) field not mandatory not support")

          case m: Msg =>
            _unpack(i + 1, buf, p + m.unpack(buf, p))
        }
      }
    }

    _unpack(0, buffer, pos)
  }

  def pack(buffer: Array[Byte], pos: Int): Int = {

    @tailrec
    def _pack(i: Int, buf: Array[Byte], p: Int): Int = {

      if (i >= fields.length) p
      else {
        fields(i) match {
          case f: Field =>
            if (format.fields(i).mandatory && !f.isSet) {
              val nm: String = format.fields(i).name
              throw new RuntimeException(s"$this.pack(buffer: Array[Byte], pos: Int) field $nm is mandatory")
            }

            if (f.isSet)
              _pack(i + 1, buf, f.pack(buf, p))
            else
              _pack(i + 1, buf, p)

          case m: Msg =>
            _pack(i + 1, buf, p + m.unpack(buf, p))
        }
      }
    }

    _pack(0, buffer, pos)
  }

  def get(id: String): Array[Byte]
  def getPack(id: String): Array[Byte]
  def getStr(id: String, formatted: Char = ' '): String

  def getField(id: String): (FieldFormat, Field) = {
    val i: Int = id.indexOf('.')

    val fieldName: String = if (i < 0) id else id.take(i)
    val index: Option[Int] = format.fieldsByName.get(fieldName)

    if (index.isDefined) {
      fields(index.get) match {
        case f: Field =>
          if (i >= 0) throw new RuntimeException(s"$this.getField(id: String) field $fieldName is invalid, expected msg")
          (format.fields(index.get).asInstanceOf[FieldFormat], f)
        case m: Msg =>
          m.getField(id.drop(i + 1))
        case _ =>
          throw new RuntimeException(s"$this.getField(id: String) addr $id not match with type")
      }
    }
    else
      throw new RuntimeException(s"$this.getField(id: String) field $id not found")
  }

  def unset(): Unit = {

    @tailrec
    def _unset(i: Int): Unit = {
      if (i < fields.length) {
        unset(format.fields(i).name)
        _unset(i + 1)
      }
    }

    _unset(0)
  }

  def unset(id: String): Unit = {
    val (_, f: Field) = getField(id)
    f.unset
  }

  def set: Int = {

    @tailrec
    def _set(i: Int, p: Int): Int = {
      if (i >= fields.length) {
        p
      }
      else {
        _set(i + 1, p + set(format.fields(i).name))
      }

    }

    _set(0, 0)
  }

  def set(id: String): Int = {
    val (fmt: FieldFormat, f: Field) = getField(id)
    if (!fmt.mandatory) {
      f.unset
      0
    }
    else
      f.set
  }

  def set(id: String, value: String): Int

  def set(id: String, value: Array[Byte]): Int

  def dump(): String
}
