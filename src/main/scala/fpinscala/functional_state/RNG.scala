package fpinscala.functional_state

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
  // 6.1 define a function that always returns a positive integer
  def nonNegativeInt(rng:RNG): (Int, RNG) = {
    val (signedInt, nextRNG) = rng.nextInt
    val int: Int = if (signedInt == Int.MinValue) 0 else signedInt.abs
    (int, nextRNG)
  }

  //6.2 define a function that returns a Double between 0 and 1(exclusive)
  def double(rng: RNG): (Double, RNG) = {
    val (posInt, nextRNG) = nonNegativeInt(rng)
    val double = posInt / (Int.MaxValue.toDouble + 1)
    (double, nextRNG)
  }

  // 6.3 define functions returning (Int,Double), (Double, Int) and (Double, Double, Double)
}
