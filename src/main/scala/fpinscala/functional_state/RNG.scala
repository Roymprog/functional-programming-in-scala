package fpinscala.functional_state

import scala.annotation.tailrec

trait RNG {

  def nextInt: (Int, RNG)

}


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[A] = State[RNG, A]

  // 6.1 define a function that always returns a positive integer
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (signedInt, nextRNG) = rng.nextInt
    val int: Int = if (signedInt == Int.MinValue) 0 else signedInt.abs
    (int, nextRNG)
  }

  def map[S, A, B](s :S => (A, S))(f: A => B): S => (B, S) = rng => {
    val (a, nextRng) = s(rng)
    (f(a), nextRng)
  }

  def nonNegativeEven: Rand[Int] = State(map(nonNegativeInt)(i => i - i %2))

  //6.2 define a function that returns a Double between 0 and 1(exclusive)
  def double(rng: RNG): (Double, RNG) = {
    val (posInt, nextRNG) = nonNegativeInt(rng)
    val double = posInt / (Int.MaxValue.toDouble + 1)
    (double, nextRNG)
  }

  def int(rng: RNG): (Int, RNG) = rng.nextInt

  def unit[A](a: A): Rand[A] = State((rng: RNG) => (a, rng))

  // 6.3 define functions returning (Int,Double), (Double, Int) and (Double, Double, Double)
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, nextRNG) = double(rng2)
    ((i, d), nextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), nextRNG) = intDouble(rng)
    ((d, i), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // 6.4 Define a function to generate a list of random ints
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def ints(count: Int)(rng: RNG)(acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) (acc, rng)
      else {
        val (i, nextRNG) = rng.nextInt
        ints(count - 1)(nextRNG)(i :: acc)
      }
    }
    ints(count)(rng)(List.empty)
  }

  // 6.5 Implement double in a more elegant way using Rand
  def elegantDouble: Rand[Double] = State(map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1)))

  // 6.6 Write a map2 function that combines 2 RNG actions
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = State(rng => {
    val (a, rng1) = ra.run(rng)
    val (b, rng2) = rb.run(rng1)
    (f(a, b), rng2)
  })

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(State(int), State(double))

  def randDoubleInt: Rand[(Double, Int)] = both(State(double), State(int))

  // 6.7 Define a function to combine a list of rng transitions
  def sequence[A](rands: List[Rand[A]]): Rand[List[A]] =
      rands.foldLeft(State.unit(List.empty): State[RNG, List[A]])(map2(_, _)((a, b) => a :+ b))

  // 6.7 Reimplement ints with sequence
  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(State((r:RNG) => r.nextInt))).run(rng)

  // 6.8 Define flatMap
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State(rng => {
    val (a, nextRNG) = f.run(rng)
    g(a).run(nextRNG)
  })

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(State((rng: RNG) => nonNegativeInt(rng)))(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) State((rng :RNG) => (mod, rng)) else nonNegativeLessThan(n)
  })

  // 6.9 Define map and map2 using flatMap
  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    val g = (a: A) => State.unit(f(a)): State[RNG, B]
    flatMap(s)(g)
  }

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    val s: Rand[(A, B)] = State((rng: RNG) => ((ra.run(rng)._1, rb.run(ra.run(rng)._2)._1 ), rb.run(ra.run(rng)._2)._2))
    val g: ((A, B)) => Rand[C] = t => State((rng: RNG) => (f(t._1, t._2), rng))
    flatMap(s)(g)
  }

  def bothByFlatMap[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2ByFlatMap(ra, rb)((_, _))

  def randIntDoubleByFlatMap: Rand[(Int, Double)] = bothByFlatMap(State(int), State(double))

  def randDoubleIntByFlatMap: Rand[(Double, Int)] = bothByFlatMap(State(double), State(int))
}