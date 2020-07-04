package fpinscala.datastructures

import scala.Option
import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_+_)

  def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_*_)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => List(h)
    case Cons(_, tail) => Cons(h, tail)
  }

  // 3.4 Assumes provided n is positive
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, tail) => if (f(h)) dropWhile(tail)(f) else tail
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => l
    case Cons(h, tail) => Cons(h, init(tail))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, tail) => f(h, foldRight(tail, z)(f))
  }

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => z + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, tail) => foldLeft(tail, f(z, h))(f)
  }

  // 3.11
  def leftsum(ints: List[Int]): Int = foldLeft(ints, 0)(_+_)
  def leftproduct(ds: List[Double]): Double = foldLeft(ds, 1.0)(_*_)

  // 3.12
  def reverse[A](as: List[A]): List[A] = foldRight(as, Nil:List[A])(Cons(_,_))

  // 3.13
  def foldLeftRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((a:A, b:B) => f(b, a))
  def foldRightLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((b:B, a:A) => f(a, b))

  // 3.14
  def append[A](a: A, as: List[A]): List[A] = foldRight(as, List(a))((a, b) => Cons(a, b))

  // 3.15
  def flatten[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil:List[A])(
      (b, a) => foldLeft(a, b)((b, a) => append(a,b))
    )

  // 3.16
  def plusone(ints: List[Int]): List[Int] = map(ints)(_ + 1)

  // 3.17
  def toString(ds: List[Double]): List[String] = map(ds)(_.toString)

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((a, b) => if (f(a)) Cons(a,b) else b)

  // 3.20
  def flatMap[A,B](as: List[A])(f:A => List[B]): List[B] = foldRight(as, Nil:List[B])((a,b) => foldRight(f(a), b)(Cons(_,_)))

  // 3.21
  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22 Assumes equal length list
  def addMatchingElements(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Cons(h1, tail1), Cons(h2, tail2)) => Cons(h1+h2, addMatchingElements(tail1, tail2))
      case _ => Nil
    }

  // 3.23
  def zipWith[A, B, C](l1: List[A],l2: List[B])(f: (A,B) => C): List[C] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(h1, tail1), Cons(h2, tail2)) => Cons(f(h1,h2), zipWith(tail1, tail2)(f))
    case _ => Nil
  }

  def addMatchingElements2(l1: List[Int], l2: List[Int]): List[Int] = List.zipWith(l1, l2)(_+_)

  def head[A](as: List[A]): Option[A] = as match {
    case Nil => None
    case Cons(h, _) => Some(h)
  }

  // 3.24 Checks whether sub is a sublist of sup
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def firstMatched(sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case Cons(_, _) if sub == Nil => true
      case Cons(h, _) if Option(h) != head(sub) => false
      case Cons(_, tail) => firstMatched(tail, drop(sub, 1))
    }

    sup match {
      case Nil => Nil == sub
      case Cons(h, tail) if Option(h) == List.head(sub) => firstMatched(tail, drop(sub, 1))
      case Cons(_, tail) => hasSubsequence(tail, sub)
    }
  }
}
