package fpinscala.generative_testing

import fpinscala.errorhandling._
import fpinscala.functional_state.RNG.{double, nonNegativeInt}
import fpinscala.functional_state.State.sequence
import fpinscala.functional_state.{RNG, SimpleRNG, State}
import fpinscala.generative_testing.Gen.buildMessage
import fpinscala.generative_testing.Prop.{MaxSize, Passed, Result, TestCases, forAll}
import fpinscala.laziness._

import scala.util.Try

object Gen {
  // 8.5 Define functions unit, boolean and listOfN
  def unit[A](a: => A): Gen[A] = Gen(State((rng: RNG) => (a, rng)))

  def boolean: Gen[Boolean] =
    Gen(State((rng: RNG) => nonNegativeInt(rng)))
      .map(i => i % 2 == 1)

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

  def intint(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).flatMap(a => choose(start, stopExclusive).map((a, _)))

  def toOption[A](gen: Gen[A]): Gen[Option[A]] =
    gen.map(Some(_))

  def fromOption[A](option: Gen[Option[A]]): Gen[A] =
    option.map {
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

  // 8.12 listOf combinator that creates lists of size by n provided to SGen
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => {
      g.listOfN(i)
    })

  // 8.13 listOf combinator that creates non-empty lists of size by n provided to SGen
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => {
      g.listOfN(i max 1)
    })

  val smallInt: Gen[Int] = Gen.choose(-10, 10)

  val maxProp: Prop = forAll(listOf1(smallInt)) { ns => {
      val max = ns.max
      !ns.exists(_ > max)
    }
  }

  // 8.14 Define properties for List.sorted

  // Sorting a list twice should return the same list
  val sortedTwice: Prop = forAll(listOf(smallInt)) {ns => {
      ns.sorted == ns.sorted.sorted
    }
  }

  // Maximum value of a sorted list should equal that of an unsorted list
  val sortedMax: Prop = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    val maxSorted = ns.sorted.max
    max == maxSorted
  }

  // The amount of elements in a sorted list should equal the unsorted list
  val sortedSize: Prop = forAll(listOf(smallInt)) { ns =>
    ns.size == ns.sorted.size
  }

  // The first element in a sorted list should be the smallest value
  val sortedMaxFirst: Prop = forAll(listOf1(smallInt)) { ns =>
    val min = ns.min
    min == ns.sorted.apply(0)
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
  def listOfN(n: Int = 100): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(State(this.sample.run(_)))))

  // 8.10 Gen -> SGen convenience method
  def unsized: SGen[A] =
    SGen(_ => this)
}

// 8.11 Some initial convenience methods for SGen
case class SGen[A](n: Int => Gen[A]) {
  def apply(i: Int): Gen[A] = n(i)

  def unit(a: A): SGen[A] =
    Gen.unit(a).unsized

  def map[B](f: A => B): SGen[B] =
    SGen(n(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val fGen: Int => Gen[B] =
      i => this(i) flatMap{f(_)(i)}

    SGen(fGen)
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(prop: Prop): Prop = Prop{
    (m, t, rng ) => run(m, t, rng) match {
      case Passed => prop.run(m, t, rng)
      case f => f
    }
  }

  // Silently ignores failure of current Prop if fails
  def ||(prop: Prop): Prop =
    Prop((m, t, rng) =>
      if (!this.run(m, t, rng).isFalsified) Passed else prop.run(m, t, rng)
    )
}

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
  type MaxSize = Int
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

  def forAll[A](gen: Gen[A])(pred: A => Boolean): Prop =
    Prop{
      (_, t, rng) =>
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

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def randomStream[A](gen: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(g => Some(gen.sample.run(g)))

  // helper to ease running a Prop
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())
         ): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
  }
}

