package fpinscala.errorhandling

import fpinscala.datastructures._

import scala.annotation.tailrec

// 4.1 Write the Option data type and basic operations map, flatMap, getorElse, orElse and filter
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]) :Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def isEmpty: Boolean = this == None
}

case class Some[+A](a: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(a))

  def flatMap[B](f: A => Option[B]): Option[B] = f(a)

  def getOrElse[B >: A](default: => B): B = a

  def orElse[B >: A](ob: => Option[B]): Option[B] = Some(a)

  def filter(f: A => Boolean): Option[A] = if (f(a)) Some(a) else None
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None

  def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  // 4.2 implement variance in terms of flatMap
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3 function that takes 2 options, a function combining both and returns an option
  def map2[A,B,C](opta: Option[A], optb: Option[B])(f: (A, B) => C): Option[C] =
    opta.flatMap(a => optb.map(f(a, _)))

  // 4.4 list of option to option of list
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(a: List[Option[A]], as : List[A]): Option[List[A]] = a match {
      case Nil => Some(as)
      case Cons(None, _) => None
      case Cons(Some(h), tail) => go(tail, Cons(h, as))
    }

    go(a, Nil)
  }

  // 4.5a
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(as: List[A], bs: List[B]): Option[List[B]] = as match {
      case Nil => Some(bs)
      case Cons(h, tail) => f(h) match {
        case None => None
        case Some(v) => go(tail, Cons(v, bs))
      }
    }

    go(as, Nil)
  }

  // 4.5b sequence in terms of traverse
  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}