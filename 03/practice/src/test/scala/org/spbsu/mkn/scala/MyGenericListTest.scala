package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.Comparators.{IntComparator, MyGenericListComparator, StringComparator}
import org.spbsu.mkn.scala.MyGenericList._

class MyGenericListTest extends AnyFunSuite {

  //remove after implementing actual MyNil

  test("head") {
    assert(fromSeq(Seq(1, 2, 3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1, 2, 3)).tail == fromSeq(Seq(2, 3)))
    assert(fromSeq(Seq(1)).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1, 2, 3)).drop(0) == fromSeq(Seq(1, 2, 3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1, 2, 3)).take(0) == MyNil)
    assert(fromSeq(Seq(1, 2, 3)).take(2) == fromSeq(Seq(1, 2)))
    assert(fromSeq(Seq(1, 2, 3)).take(3) == fromSeq(Seq(1, 2, 3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).take(10))
  }

  test("map") {
    assert((MyNil : MyGenericList[Int]).map(_ * 2) == MyNil)
    assert(fromSeq(Seq(1, 2, 3)).map(_ * 2) == fromSeq(Seq(2, 4, 6)))
    assert(fromSeq(Seq(1, 2, 3)).map(identity) == fromSeq(Seq(1, 2, 3)))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1, 2, 3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum(MyNil : MyGenericList[Int]))
    assert(sum(fromSeq(Seq(1, 2, 3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)
  }

  test("sort") {
    assert(sort(MyNil : MyGenericList[Int]) == MyNil)
    assert(sort(fromSeq(Seq(42, 1, 87, -31, 1203, 11))) == fromSeq(Seq(-31, 1, 11, 42, 87, 1203)))
    assert(sort(fromSeq(Seq("bazooka","foo", "bar", "baz", "azimut"))) ==
      fromSeq(Seq("azimut","bar", "baz", "bazooka", "foo")))
    val a = 6 :: 1 :: 89 :: 2 :: 5 :: 6 :: -100 :: MyNil
    val b = 6 :: 1 :: 89 :: 2 :: MyNil
    val c = 6 :: -34 :: 123 :: MyNil
    assert(sort(a :: b :: c :: MyNil) == c :: b :: a :: MyNil)
  }
}
