package org.spbsu.mkn.scala

import java.util.Comparator

import org.spbsu.mkn.scala.MyGenericList._

import scala.annotation.tailrec

sealed trait MyGenericList[+A] {
  def withFilter(p: A => Boolean) : MyGenericList[A]

  def head: A

  def tail: MyGenericList[A]

  def drop(n: Int): MyGenericList[A]

  def take(n: Int): MyGenericList[A]

  def map[B](f: A => B): MyGenericList[B]

  def ::[B >: A](elem: B): MyGenericList[B] = new ::(elem, this)

  def ++[B >: A](right: MyGenericList[B]) : MyGenericList[B] = foldRight(right, this)((a, b) => a :: b)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef

  override def tail: MyGenericList[Nothing] = undef

  override def drop(n: Int): MyGenericList[Nothing] = if (n <= 0) this else undef

  override def take(n: Int): MyGenericList[Nothing] = if (n <= 0) MyNil else undef

  override def map[B](f: Nothing => B): MyGenericList[B] = MyNil

  override def withFilter(p: Nothing => Boolean): MyGenericList[Nothing] = MyNil

}


case class ::[+A](head: A, tail: MyGenericList[A]) extends MyGenericList[A] {
  override def drop(n: Int): MyGenericList[A] = if (n <= 0) this else tail.drop(n - 1)

  override def take(n: Int): MyGenericList[A] = if (n <= 0) MyNil else head :: tail.take(n - 1)

  override def map[B](f: A => B): MyGenericList[B] = f(head) :: tail.map(f)

  override def withFilter(p: A => Boolean): MyGenericList[A] = if (p(head)) head :: tail.withFilter(p) else tail.withFilter(p)
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


  def sort[A](list : MyGenericList[A])(implicit eq: Comparator[A]) : MyGenericList[A] = {
    list match {
      case MyNil => MyNil
      case x :: xs =>
        val left = for { a <- xs if eq.compare(a, x) < 0 } yield a
        val right = for { a <- xs if eq.compare(a, x) >= 0 } yield a
        sort(left)(eq) ++ (x :: sort(right)(eq))
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

  def foldRight[A, B](ini: B, list: MyGenericList[A])(f: (A, B) => B): B = {
    list match {
      case MyNil => ini
      case x :: xs => f(x, foldRight(ini, xs)(f))
    }
  }

  @tailrec
  def compare[A](o1 : MyGenericList[A], o2 : MyGenericList[A])(implicit eq: Comparator[A]) : Int = {
    (o1, o2) match {
      case (MyNil, MyNil) => 0
      case (_ :: _, MyNil) => 1
      case (MyNil, _ :: _) => -1
      case (x :: xs, y :: ys) =>
        val cmp = eq.compare(x, y)
        if(cmp == 0) compare(xs, ys) else cmp
    }
  }
}

object Comparators {
  implicit val IntComparator: Comparator[Int] = (o1: Int, o2: Int) => o1 - o2
  implicit val StringComparator: Comparator[String] = (o1: String, o2: String) => o1.compareTo(o2)
  implicit def MyGenericListComparator[A](implicit eq : Comparator[A]): Comparator[MyGenericList[A]] =
    (o1: MyGenericList[A], o2: MyGenericList[A]) => compare(o1, o2)
}
