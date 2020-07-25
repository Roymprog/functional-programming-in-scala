package fpinscala.errorhandling

import fpinscala.datastructures._

import scala.annotation.tailrec

// 4.6 Write map, flatMap, orElse and map2 for Either
sealed trait Either[+E, +A] {
  def map[B] (f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  def map[B](f: Nothing => B): Either[E, B] = Left(value)

  def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = Left(value)

  def orElse[EE >: E, B >: Nothing](b: =>Either[EE, B]): Either[EE, B] = b

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = Left(value)
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

  def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)

  def orElse[EE >: Nothing, B >: A](b: =>Either[EE, B]): Either[EE, B] = this

  def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(f(value, _))
}

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum/xs.length)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}

  // 4.7 implement Either sequence and traverse functions
  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = {
    @tailrec
    def go(es: List[Either[E,A]], acc: List[A]): Either[E, List[A]] = es match {
      case Nil => Right(acc)
      case Cons(Left(e), _) => Left(e)
      case Cons(Right(h), tail) => go(tail, Cons(h, acc))
    }

    go(es, Nil)
  }

  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    @tailrec
    def go(as: List[A], acc: List[B]): Either[E, List[B]] = as match {
      case Nil => Right(acc)
      case Cons(h, tail) => f(h) match {
        case Left(v) => Left(v)
        case Right(v) => go(tail, Cons(v, acc))
      }
    }

    go(as, Nil)
  }
}