package redbook.chapter3

import scala.annotation.tailrec

// `List` data type, parameterized on a type, `A`
sealed trait List[+A]

// A `List` data constructor representing the empty list
final case object Nil extends List[Nothing]

// Another data constructor, representing nonempty lists.
// Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
final case class Cons[+A](head: A, tail: List[A]) extends List[A]

// `List` companion object. Contains functions for creating and working with lists.
object List {

  // A function that uses pattern matching to add up a list of integers
  def sum(ints: List[Int]): Int = ints match {
    // The sum of the empty list is 0.
    case Nil => 0
    // The sum of a list starting with `headElement` is `headElement` plus the sum of the rest of the list.
    case Cons(headElement, restOfList) => headElement + sum(restOfList)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  // Variadic function syntax
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // Utility functions
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => List()
    case Cons(_,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_,rest) => Cons(h,rest)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t,f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def sumLeft(ns: List[Int]): Int = ???

  def productLeft(ns: List[Double]): Double = ???

  def lengthLeft[A](ns: List[A]): Int = ???

  def reverse[A](ns: List[A]): List[A] = ???

  def appendFold[A](a1: List[A], a2: List[A]): List[A] = ???

  def flatten[A](ns: List[List[A]]): List[A] = ???
}
