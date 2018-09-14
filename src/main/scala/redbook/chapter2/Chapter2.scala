package redbook.chapter2

import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {
    // 0 1 2 3 4 5
    // 0 1 1 2 3 5
    // ^ ^ ^ ^ ^ ^
//pres x 0 1 1 2 3
//prev x x 0 1 1 2

    @tailrec
    def go(n: Int, present:Int, previous:Int): Int = {
      if(n>0)
        go(n - 1, present + previous, present)
      else
        present+previous
    }

    n match {
      case 0 => 0
      case 1 => 1
      case _ => go(n-1, 0, 1)
    }
  }


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def internalIsSorted(current:A,as: Array[A]): Boolean = {
      as.headOption match {
        case Some(head) =>
          if (!ordered.apply(current, head))
            false
          else
            internalIsSorted(head, as.tail)
        case None => true
      }
    }

    as.headOption.fold(true)(internalIsSorted(_, as.tail))
  }


  def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)


  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)


  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
