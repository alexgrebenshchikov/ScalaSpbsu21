package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList
}

case object IntNil extends IntList {
  override def ::(elem: Int): IntList = Cons(elem, IntNil)

  override def head: Int = undef

  override def tail: IntList = undef

  override def drop(n: Int): IntList = if (n <= 0) this else undef

  override def take(n: Int): IntList = if (n <= 0) IntNil else undef

  override def map(f: Int => Int): IntList = IntNil
}

case class Cons(head: Int, tail: IntList) extends IntList {
  override def ::(elem: Int): IntList = Cons(elem, this)

  override def drop(n: Int): IntList = if (n <= 0) this else tail.drop(n - 1)

  override def take(n: Int): IntList = if (n <= 0) IntNil else Cons(head, tail.take(n - 1))

  override def map(f: Int => Int): IntList = Cons(f(head), tail.map(f))
}

object IntList {
  def apply(elem: Int*): IntList = if (elem.isEmpty) IntNil else Cons(elem.head, apply(elem.tail: _*))

  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = {
    seq match {
      case x +: xs => Cons(x, fromSeq(xs))
      case _ => IntNil
    }
  }

  def sum(intList: IntList): Int = {
    intList match {
      case IntNil => undef
      case _ => foldLeft(intList, 0)((a, b) => a + b)
    }
  }

  def size(intList: IntList): Int = {
    foldLeft(intList, 0)((s, _) => s + 1)
  }

  // extra task: implement sum using foldLeft
  @tailrec
  def foldLeft[B](list: IntList, ini: B)(f: (B, Int) => B): B = {
    list match {
      case IntNil => ini
      case Cons(x, xs) => foldLeft(xs, f(ini, x))(f)
    }
  }
}
