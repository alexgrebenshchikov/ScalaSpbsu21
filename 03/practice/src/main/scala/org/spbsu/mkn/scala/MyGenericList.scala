package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.annotation.tailrec
import scala.math.Numeric.BigDecimalAsIfIntegral.plus

sealed trait MyGenericList[+A] {
  def head: A

  def tail: MyGenericList[A]

  def drop(n: Int): MyGenericList[A]

  def take(n: Int): MyGenericList[A]

  def map[B](f: A => B): MyGenericList[B]

  def ::[B >: A](elem: B): MyGenericList[B] = new ::(elem, this)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef

  override def tail: MyGenericList[Nothing] = undef

  override def drop(n: Int): MyGenericList[Nothing] = if (n <= 0) this else undef

  override def take(n: Int): MyGenericList[Nothing] = if (n <= 0) MyNil else undef

  override def map[B](f: Nothing => B): MyGenericList[B] = MyNil
}


case class ::[+A](head: A, tail: MyGenericList[A]) extends MyGenericList[A] {
  override def drop(n: Int): MyGenericList[A] = if (n <= 0) this else tail.drop(n - 1)

  override def take(n: Int): MyGenericList[A] = if (n <= 0) MyNil else head :: tail.take(n - 1)

  override def map[B](f: A => B): MyGenericList[B] = f(head) :: tail.map(f)
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[A](seq: Seq[A]): MyGenericList[A] = seq.foldRight(MyNil: MyGenericList[A])((elem, tail) => elem :: tail)

  def size[A](list: MyGenericList[A]): Int = foldLeft(0, list)((acc, _) => acc + 1)

  def sum[A, B >: A](list: MyGenericList[A])(implicit num: math.Numeric[B]) : B = {
    list match {
      case MyNil => undef
      case _ :: _ => foldLeft(num.zero, list)((acc, x) => num.plus(acc, x))
    }
  }

  // extra task: implement sum using foldLeft
  @tailrec
  def foldLeft[A, B](ini: B, list: MyGenericList[A])(f: (B, A) => B): B = {
    list match {
      case MyNil => ini
      case x :: xs => foldLeft(f(ini, x), xs)(f)
    }
  }
}

