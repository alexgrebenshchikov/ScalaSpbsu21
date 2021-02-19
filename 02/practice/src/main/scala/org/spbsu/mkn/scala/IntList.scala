package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int = {
    this match {
      case IntNil => undef
      case Cons(x, _) => x
    }
  }
  def tail: IntList = {
    this match {
      case IntNil => undef
      case Cons(_, xs) => xs
    }
  }

  def drop(n: Int): IntList = if (n <= 0) this else tail.drop(n - 1)

  def take(n: Int): IntList = if (n <= 0) IntNil else Cons(head, tail.take(n - 1))

  def map(f: Int => Int): IntList = {
    this match {
      case IntNil => IntNil
      case Cons(x, xs) => Cons(f(x), xs.map(f))
    }
  }
  def ::(elem: Int): IntList
}

case object IntNil extends IntList {
  override def ::(elem: Int): IntList = Cons(elem, IntNil)
}

case class Cons(hd: Int, tl: IntList) extends IntList {
  override def ::(elem: Int): IntList = Cons(elem, hd :: tl)
}

object IntList {
  def apply(elem: Int*): IntList = if(elem.isEmpty) IntNil else Cons(elem.head, apply(elem.tail: _*))
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
      case Cons(x, IntNil) => x
      case Cons(x, xs) => x + sum(xs)
    }
  }
  def size(intList: IntList): Int = {
    intList match {
      case IntNil => 0
      case Cons(_, xs) => 1 + size(xs)
    }
  }
  // extra task: implement sum using foldLeft
  @tailrec
  def foldLeft[B](list : IntList, ini : B)(f : (B, Int) => B): B = {
    list match {
      case IntNil => ini
      case Cons(x, xs) => foldLeft(xs, f(ini, x))(f)
    }
  }

  def sum2(list : IntList) : Int = {
    foldLeft(list, 0)((a, b) => a + b)
  }
}


