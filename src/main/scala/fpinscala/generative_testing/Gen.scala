package fpinscala.generative_testing

import fpinscala.errorhandling._
import fpinscala.functional_state.RNG.{double, nonNegativeInt}
import fpinscala.functional_state.State.sequence
import fpinscala.functional_state.{RNG, State}
import fpinscala.generative_testing.Prop.{Falsified, Passed, Result, TestCases}
import fpinscala.laziness._

import scala.util.Try

object Gen {
  // 8.5 Define functions unit, boolean and listOfN
  def unit[A](a: => A): Gen[A] = Gen(State((rng: RNG) => (a, rng)))

  def boolean: Gen[Boolean] =
    Gen(State((rng: RNG) => nonNegativeInt(rng)))
      .map(i => i % 2 == 1)

  def forAll[A](gen: Gen[A])(pred: A => Boolean): Prop =
    Prop{
      (t:TestCases, rng: RNG) =>
        randomStream(gen)(rng)
          .unfoldZipWith(Stream.from(0))
          .take(t)
          .map {
            case (a, i) => Try(
              if (pred(a)) Passed else Falsified(a.toString, i)
            ).fold(e => Falsified(buildMessage(a, e), i), s => s)
          }
          .toList
          .find(_.isFalsified).getOrElse(Passed)
    }
//  {
//    val run = (t: TestCases, rng: RNG) => {
//      gen.listOfN(t)
//        .map(_.map(pred)) // run predicate
//    }.map(bools => (bools, bools.count(b => b))) // count success cases
//      .map{
//        t => if (t._1.size == t._2) Passed else Falsified("failed", (t._1.size - t._2))
//      }.sample.run(rng)._1
//
//    Prop(run)
//  }

  def randomStream[A](gen: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(g => Some(gen.sample.run(g)))

  def buildMessage[A](s: A, e: Throwable): String =
    s"Test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace}"

  // 8.4 Define function Gen.choose that generates numbers between start and stopExclusive
  // Assuming start >= 0 and stopExclusive > start
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(
      RNG.map((rng: RNG) => nonNegativeInt(rng))(i => i % (stopExclusive - start) + start)
    ))

  def intint(start: Int, stopExclusive:Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).flatMap(a => choose(start, stopExclusive).map((a, _)))

  def toOption[A](gen: Gen[A]): Gen[Option[A]] =
    gen.map(Some(_))

  def fromOption[A](option: Gen[Option[A]]) =
    option.map{
      case Some(value) => value
      case None => throw new IllegalArgumentException("Cannot handle None")
    }

  def char: Gen[Char] =
    choose(0, Int.MaxValue).map(_.toChar)

  def string(length: Int): Gen[String] =
    char
      .listOfN(length)
      .map(_.mkString)

  // 8.7 Define function that pulls values from two generators with equal likelihood
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  // 8.8 Define function that pulls values from two generators with equal likelihood based
  // on relative weights. Assumes double values are positive
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val ratio = g1._2 / (g1._2 + g2._2)
    Gen(State((rng: RNG) => {
      double(rng)
    }))
      .flatMap(random => if (random <= ratio) g1._1 else g2._1)
  }

}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(this.sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(State(
      (rng: RNG) => {
        val state = this.sample.run(rng)
        f(state._1).sample.run(state._2)
      }
    ))

  // Use a list of 100 elements as default
  def listOfN(n: Int = 100): Gen[List[A]] = {
    val list: List[State[RNG, A]] =
      (0 to n).map(_ => State(this.sample.run(_))).toList
    val randlist: State[RNG, List[A]] = sequence(list)
    Gen(State((rng: RNG) => randlist.run(rng)))
  }
}

case class Prop(run: (TestCases, RNG) => Result)

trait PropBool {
  def check: Boolean
  // 8.3 Implement Prop as a Boolean only signalling success or failure
  def &&(p: PropBool): PropBool = {
    val thisProp = this
    new PropBool {
      override def check: Boolean = thisProp.check && p.check
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }
}