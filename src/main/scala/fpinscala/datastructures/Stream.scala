package fpinscala.laziness

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
      case Cons(h, tail) => go(tail(), Stream.cons(h(), acc), n - 1)
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
          case Cons(h, tail) => go(tail(), Stream.cons(h(), acc), f)
        }

    go(this, Empty, f)
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

}