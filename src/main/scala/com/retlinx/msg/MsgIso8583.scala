/**
  * Retlinx Message Processor
  *
  * ISO8583 Implementation
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg

import com.retlinx.msg.field.Field
import com.retlinx.msg.format.{FieldFormat, MsgFormat}

import scala.annotation.tailrec


class MsgIso8583(override protected val format: MsgFormat,
                 override val usingBuffer: Boolean = false) extends Msg(format, usingBuffer) {

  private[this] val mandatoryList: Array[String] = Array("MTI", "BITMAP1", "BITMAP2", "BITMAP3")
  required()

  override lazy val data: Array[Byte] = if (!usingBuffer) null else Array.fill[Byte](format.maxLength)(0x00.toByte)

  private[this] val bitmap: Array[Char] = Array.fill[Char](192)('0')


  private[this] def required(): Unit = {
    mandatoryList.foreach(n => {
      val index: Option[Int] = format.fieldsByName.get(n)
      assert(index.isDefined, s"$this.require $n field ")
    })
  }

  override def unpack(buffer: Array[Byte], pos: Int): Int = {

    def _unpack4(i: Int, b: Array[Byte], p: Int): Int = {
      if (i >= 4) p
      else {
        val name: String = mandatoryList(i)
        val index: Option[Int] = format.fieldsByName.get(name)
        if (index.isDefined) {
          i match {
            case 0 =>
              _unpack4(i + 1, b, fields(index.get).unpack(b, p))

            case 1 =>
              val np: Int = fields(index.get).unpack(b, p)
              val bmap: Array[Char] = fields(index.get).asInstanceOf[Field].toStr('b').toArray.drop(2)
              Array.copy(bmap, 0, bitmap, 0, 64)
              _unpack4(i + 1, b, np)

            case 2 =>
              if (bitmap(0) == '1')
              {
                val np: Int = fields(index.get).unpack(b, p)
                val bmap: Array[Char] = fields(index.get).asInstanceOf[Field].toStr('b').toArray.drop(2)
                Array.copy(bmap, 0, bitmap, 64, 64)
                _unpack4(i + 1, b, np)
              }
              else
              {
                p
              }

            case 3 =>
              if (bitmap(64) == '1')
              {
                val np: Int = fields(index.get).unpack(b, p)
                val bmap: Array[Char] = fields(index.get).asInstanceOf[Field].toStr('b').toArray.drop(2)
                Array.copy(bmap, 0, bitmap, 128, 64)
                _unpack4(i + 1, b, np)
              }
              else
              {
                p
              }
          }
        }
        else
          throw new RuntimeException(s"$this->pack(buffer: Array[Byte], pos: Int) field $name mandatory")
      }
    }

    @tailrec
    def _unpack(i: Int, b: Array[Byte], p: Int): Int = {

      val stop: Boolean = if (i >= 192)
        true
      else if (i >= 128 && bitmap(64) != '1')
        true
      else if (i >= 64 && bitmap(0) != '1')
        true
      else
        false

      if (stop)
        p
      else {
        val skip: Boolean = i == 0 || i == 64 || bitmap(i) == '0'

        if (skip) {
          _unpack(i + 1, b, p)
        }
        else {
          val n: String = "F%03d".format(i + 1)
          val (_, f: Field) = getField(n)

          _unpack(i + 1, b, f.unpack(b, p))
        }
      }
    }

    _unpack(0, buffer, _unpack4(0, buffer, pos))
  }

  override def pack(buffer: Array[Byte], pos: Int = 0): Int = {

    @tailrec
    def _pack4(i: Int, b: Array[Byte], p: Int): Int = {
      if (i >= 4) p
      else {
        val name: String = mandatoryList(i)
        val index: Option[Int] = format.fieldsByName.get(name)
        if (index.isDefined) {
          i match {
            case 0 =>
              if (fields(index.get).isSet)
                _pack4(i + 1, b, fields(0).pack(b, p))
              else
                throw new RuntimeException(s"$this->pack(buffer: Array[Byte], pos: Int) field $name not set")

            case 1 =>
              if (bitmap.slice(64, 128).forall(b => b == '0'))
                bitmap(0) = '0'
              else
                bitmap(0) = '1'

              fields(1).asInstanceOf[Field].set("b|" + bitmap.slice(0, 64).mkString)
              _pack4(i + 1, b, fields(1).pack(b, p))

            case 2 =>
              if (bitmap(0) == '1') {
                if (bitmap.slice(128, 192).forall(b => b == '0'))
                  bitmap(64) = '0'
                else
                  bitmap(64) = '1'

                fields(2).asInstanceOf[Field].set("b|" + bitmap.slice(64, 128).mkString)
                _pack4(i + 1, b, fields(2).pack(b, p))
              }
              else
              {
                _pack4(i + 1, b, p)
              }

            case 3 =>
              if (bitmap(64) == '1') {
                fields(3).asInstanceOf[Field].set("b|" + bitmap.slice(128, 192).mkString)
                _pack4(i + 1, b, fields(3).pack(b, p))
              }
              else
              {
                _pack4(i + 1, b, p)
              }
          }
        }
        else
          throw new RuntimeException(s"$this->pack(buffer: Array[Byte], pos: Int) field $name mandatory")
      }
    }

    @tailrec
    def _pack(i: Int, b: Array[Byte], p: Int): Int = {

      val skip: Boolean = i == 0 || i == 64

      val stop: Boolean = if (i >= 192)
        true
      else if (i >= 128 && bitmap(64) != '1')
        true
      else if (i >= 64 && bitmap(0) != '1')
        true
      else
        false

      if (stop)
        p
      else if (skip)
        _pack(i + 1, b, p)
      else
      if (bitmap(i) == '1') {
        val n: String = "F%03d".format(i + 1)
        val (_, f: Field) = getField(n)
        if (f.isSet) {
          _pack(i + 1, b, f.pack(b, p))
        }
        else
          throw new RuntimeException(s"$this->pack(buffer: Array[Byte], pos: Int) field $n not set but bit indicator is on")
      }
      else
      {
        _pack(i + 1, b, p)
      }
    }

    _pack(1, buffer, _pack4(0, buffer, pos))
  }

  private[this] def setBit(id: String, on: Boolean): Unit = {
    if (id.length == 4 && id(0) == 'F') {
      val num: String = id.drop(1)
      if (num.matches("[0-9]+")) {
        val index: Int = num.toInt - 1
        if (on)
          bitmap(index) = '1'
        else
          bitmap(index) = '0'
      }
    }
  }

  override def get(id: String): Array[Byte] = {
    val (fmt: FieldFormat, f: Field) = getField(id)

    if (fmt.name.equals(mandatoryList(1))) {
      if (bitmap.slice(64, 128).forall(b => b == '0'))
        bitmap(0) = '0'
      else
        bitmap(0) = '1'

      f.set("b|" + bitmap.slice(0, 64).mkString)
    }
    else if (fmt.name.equals(mandatoryList(2))) {
      if (bitmap.slice(128, 192).forall(b => b == '0'))
        bitmap(64) = '0'
      else
        bitmap(64) = '1'

      f.set("b|" + bitmap.slice(64, 128).mkString)
    }
    else if (fmt.name.equals(mandatoryList(3))) {
      f.set("b|" + bitmap.slice(128, 192).mkString)
    }

    f.getBytes
  }

  override def getPack(id: String): Array[Byte] = {
    val (fmt: FieldFormat, f: Field) = getField(id)

    if (fmt.name.equals(mandatoryList(1))) {
      if (bitmap.slice(64, 128).forall(b => b == '0'))
        bitmap(0) = '0'
      else
        bitmap(0) = '1'

      f.set("b|" + bitmap.slice(0, 64).mkString)
    }
    else if (fmt.name.equals(mandatoryList(2))) {
      if (bitmap.slice(128, 192).forall(b => b == '0'))
        bitmap(64) = '0'
      else
        bitmap(64) = '1'

      f.set("b|" + bitmap.slice(64, 128).mkString)
    }
    else if (fmt.name.equals(mandatoryList(3))) {
      f.set("b|" + bitmap.slice(128, 192).mkString)
    }

    f.getBytes
  }

  override def getStr(id: String, formatted: Char = ' '): String = {
    val (fmt: FieldFormat, f: Field) = getField(id)

    if (fmt.name.equals(mandatoryList(1))) {
      if (bitmap.slice(64, 128).forall(b => b == '0'))
        bitmap(0) = '0'
      else
        bitmap(0) = '1'

      f.set("b|" + bitmap.slice(0, 64).mkString)
    }
    else if (fmt.name.equals(mandatoryList(2))) {
      if (bitmap.slice(128, 192).forall(b => b == '0'))
        bitmap(64) = '0'
      else
        bitmap(64) = '1'

      f.set("b|" + bitmap.slice(64, 128).mkString)
    }
    else if (fmt.name.equals(mandatoryList(3))) {
      f.set("b|" + bitmap.slice(128, 192).mkString)
    }

    f.toStr(formatted)
  }

  override def unset(id: String): Unit = {
    val (fmt: FieldFormat, f: Field) = getField(id)
    if (fmt.name.equals("MTI")) {
      f.set
    }
    else {
      if (mandatoryList.drop(1).forall(e => !e.equals(fmt.name))) {
        setBit(id, on = false)
        f.unset
      }
    }
  }

  override def set(id: String): Int = {
    val (fmt: FieldFormat, f: Field) = getField(id)

    if (fmt.name.equals("MTI")) {
      f.set
    }
    else {
      if (mandatoryList.drop(1).exists(e => !e.equals(fmt.name))) {
        0
      }
      else {
        if (!fmt.mandatory) {
          setBit(id, on = false)
          f.unset
          0
        }
        else {
          setBit(id, on = true)
          f.set
        }
      }
    }
  }

  override def set(id: String, value: String): Int = {
    val (fmt: FieldFormat, f: Field) = getField(id)

    if (fmt.name.equals("MTI")) {
      f.set(value)
    }
    else {
      if (mandatoryList.drop(1).exists(e => !e.equals(fmt.name))) {
        0
      }
      else {
        setBit(id, on = true)
        f.set(value)
      }
    }
  }

  override def set(id: String, value: Array[Byte]): Int = {
    val (fmt: FieldFormat, f: Field) = getField(id)

    if (fmt.name.equals("MTI")) {
      f.unpack(value, 0)
    }
    else {
      if (mandatoryList.drop(1).exists(e => !e.equals(fmt.name))) {
        0
      }
      else {
        setBit(id, on = true)
        f.unpack(value, 0)
      }
    }
  }

  override def dump(): String = {

    def _append(s: StringBuilder, xs: String, l: Int, indent: Int): Unit = {
      for (i <- 0 until indent) s.append(' ')
      s.append(xs)
      for (i <- xs.length until l) s.append(' ')
    }

    @tailrec
    def _dump4(i: Int, s: StringBuilder, indent: Int): Unit = {

      if (i < 4) {
        val (fmt: FieldFormat, f: Field) = getField(mandatoryList(i))
        val write: Boolean = {
          i match {
            case 1 =>
              if (bitmap.slice(64, 128).forall(b => b == '0'))
                bitmap(0) = '0'
              else
                bitmap(0) = '1'

              fields(1).asInstanceOf[Field].set("b|" + bitmap.slice(0, 64).mkString)
              true

            case 2 =>
              if (bitmap.slice(128, 192).forall(b => b == '0'))
                bitmap(64) = '0'
              else
                bitmap(64) = '1'

              fields(2).asInstanceOf[Field].set("b|" + bitmap.slice(64, 128).mkString)
              if (bitmap(0) != '1')
                false
              else
                true

            case 3 =>
              fields(3).asInstanceOf[Field].set("b|" + bitmap.slice(128, 192).mkString)
              if (bitmap(64) != '1')
                false
              else
                true

            case _ => true
          }
        }
        if (write) {
          _append(s, fmt.name, 10, indent)
          s.append(" :  ")
          s.append(f.toStr('x'))
          s.append('\n')
        }
        _dump4(i + 1, s, indent)
      }
    }

    def _append2(s: StringBuilder, xs: String, l: Int, indent: Int, col: Int): Unit = {


      if (xs.length < col) {
        s.append(' ')
        s.append(xs)
        s.append('\n')
      }
      else {
        val a: Array[String] = xs.sliding(col, col).toArray
        for(i <- a.indices) {
          if (i != 0) {
            for (j <- 0 until (indent + l + 3)) s.append(' ')
          }
          s.append('|')
          s.append(a(i))
          s.append('|')
          s.append('\n')
        }
      }

    }

    @tailrec
    def _dump(i: Int, s: StringBuilder, indent: Int): Unit = {

      val stop: Boolean = if (i >= 192)
        true
      else if (i >= 128 && bitmap(64) != '1')
        true
      else if (i >= 64 && bitmap(0) != '1')
        true
      else
        false

      if (!stop) {
        val skip: Boolean = i == 0 || i == 64 || bitmap(i) == '0'

        if (!skip) {
          val (fmt: FieldFormat, f: Field) = getField("F%03d".format(i + 1))

          _append(s, fmt.name, 10, indent)
          s.append(" : ")
          if (f.isSet)
            _append2(s, f.toStr('x'), 10, indent, 40)
          else
            s.append("ERROR - NOT SET\n")
        }
        _dump(i + 1, s, indent)
      }
    }

    val sb: StringBuilder = StringBuilder.newBuilder
    _dump4(0, sb, 0)
    _dump(1, sb, 0)
    sb.toString()
  }

  override def toString: String = "[MSGISO]"
}
