package fpinscala.generative_testing

import fpinscala.functional_state.RNG.nonNegativeInt
import fpinscala.functional_state.State.sequence
import fpinscala.functional_state.{RNG, SimpleRNG, State}
import fpinscala.generative_testing.Prop.{FailedCase, SuccessCount}

object Gen {
  // 8.5 Define functions unit, boolean and listOfN
  def unit[A](a: => A): Gen[A] = Gen(State((rng: RNG) => (a, rng)))

  def boolean: Gen[Boolean] = Gen(State(RNG.map((rng: RNG) => nonNegativeInt(rng))(i => i % 2 == 1)))

  // Use a list of 100 elements as default
  def listOf[A](a: Gen[A]): Gen[List[A]] = listOfN(100, a)

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = {
    val list: List[State[RNG, A]] =
      (0 to n).map(_ => State(a.sample.run(_))).toList
    val randlist: State[RNG, List[A]] = sequence(list)
    Gen(State((rng: RNG) => randlist.run(rng)))
  }

  def forAll[A](gen: Gen[A])(pred: A => Boolean): Prop = ???

  // 8.4 Define function Gen.choose that generates numbers between start and stopExclusive
  // Assuming start >= 0 and stopExclusive > start
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state = State(RNG.map((rng: RNG) => nonNegativeInt(rng))(i => i % (stopExclusive - start) + start))
    Gen(state)
  }
}

case class Gen[A](sample: State[RNG, A])

trait Prop {
  def check: Either[(FailedCase, SuccessCount),SuccessCount]
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
  type FailedCase = String
  type SuccessCount = Int
}