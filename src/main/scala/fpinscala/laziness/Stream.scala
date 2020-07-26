package fpinscala.laziness

import fpinscala.errorhandling._
import fpinscala.laziness.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A] {
  // 5.1 define list method to inspect Stream
  def toList: List[A] = {
    @tailrec
    def go(as: Stream[A], aslist: List[A]): List[A] = as match {
      case Empty => aslist
      case Cons(h, tail) => go(tail(), aslist :+ h())
    }

    go(this, scala.Nil)
  }

  // 5.2 define take and drop
  // assumes n >= 0
  def take(n: Int): Stream[A] = {
    @tailrec
    def go(as: Stream[A], acc: Stream[A], n: Int): Stream[A] =
      if (n == 0) acc.reverse
      else
      as match {
      case Empty => acc.reverse
      case Cons(h, tail) => go(tail(), cons(h(), acc), n - 1)
    }

    go(this, Empty, n)
  }

  def reverse: Stream[A] = {
    @tailrec
    def go(as: Stream[A], acc: Stream[A]): Stream[A] = as match {
      case Empty => acc
      case Cons(h, tail) => go(tail(), Cons(h, () => acc))
    }

    go(this, Empty)
  }

  // assumes n >= 0
  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Empty => this
      case Cons(_, tail) => tail().drop(n -1)
    }

  // 5.3 define takewhile
  def takeWhile(f: A => Boolean): Stream[A] = {
    @tailrec
    def go(as: Stream[A], acc: Stream[A], f: A => Boolean): Stream[A] =
        as match {
          case Empty => acc.reverse
          case Cons(h, _) if !f(h()) => acc.reverse
          case Cons(h, tail) => go(tail(), cons(h(), acc), f)
        }

    go(this, Empty, f)

  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, tail) => f(h(), tail().foldRight(z)(f))
  }

  def exists(f: A => Boolean): Boolean = this.foldRight(false)((a, b) => f(a) || b)

  // 5.4 implement forAll
  def forAll(f: A => Boolean): Boolean = this.foldRight(true)((a, b) => f(a) && b)

  // 5.5 takewhile using foldRight
  def takeWhileFR(f: A => Boolean): Stream[A] = this.foldRight(empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  // 5.6 implement headOption
  def headOption: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 implement map, filter, append, and flatMap using foldRight
  def map[B](f: A => B): Stream[B] = this.foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](append: => Stream[B]): Stream[B] = this.foldRight(append)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  // 5.13 define map, take, takeWhile, zipWith and zipAll using unfold
  def unfoldMap[B](f: A => B): Stream[B] = unfold(this)(s => {
    s match {
      case Empty => None
      case Cons(h, tail) => Some((f(h()), tail()))
    }
  })

  def unfoldTake(n: Int) : Stream[A] = unfold(this){
    case Empty => None
    case _ if n == 0 => None
    case Cons(h, tail) => Some((h(), tail().unfoldTake(n - 1)))
  }

  def unfoldTakeWhile(f: A => Boolean): Stream[A] = unfold(this){
    case Empty => None
    case Cons(h, _) if !f(h()) => None
    case Cons(h, tail) => Some((h(), tail().unfoldTakeWhile(f)))
  }

  def unfoldZipWith[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)){
    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(a, tail1), Cons(b, tail2)) => Some((a(), b()), (tail1(), tail2()))
  }
}

case class Cons[+A](h: () => A, tail: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // 5.8 define function to generate infinite stream of arbitrary type and value
  def constant[A](a: A): Stream[A] = Stream.cons(a, Stream.constant(a))

  // 5.9 define function to generate infinite stream of integers starting with n, then n + 1, n + 2 etc.
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // 5.10 define a function that returns an infinite Fibonacci stream
  def fib: Stream[Int] = {
    def fib(n: Int, prev: Int): Stream[Int] = Stream.cons(n, fib(n + prev, n))
    Stream.cons(0, fib(1, 0))
  }

  // 5.11 define unfold stream building function
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  // 5.12 define fibs, from, constant (and ones) as unfold
  def unfoldConstant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def unfoldFrom(n: Int): Stream[Int] = unfold(n)(_ => Some((n, n+1)))

  def unfoldFibs: Stream[Int] = unfold((0,1)){case (f0, f1) => Some(f0, (f1, f0 + f1))}

}