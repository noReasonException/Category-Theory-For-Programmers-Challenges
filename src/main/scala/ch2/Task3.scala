package ch2

import scala.util.Random

object Task3 {

  // Most random number generators can be initialized with a seed.Implement a function that takes a seed, calls the
  // random number generator with that seed, and returns the result. Memoize that function. Does it work?
  // Well it works now, as Seed -> Result is deterministic.
  def main(args: Array[String]): Unit = {
    val nonMemoized: Int => Int = new Random(_).nextInt()
    val memoized: Int => Int    = memoize(nonMemoized)

    val nonMemoizedResult = (1 to 10).map(nonMemoized)
    val memoizedResult    = (1 to 10).map(memoized)

    println(nonMemoizedResult)
    println(memoizedResult)

  }

  def memoize[A, B](f: A => B): A => B = {
    val map = scala.collection.mutable.Map[A, B]()
    a: A => {
      if (map.exists(_._1 == a)) {
        map(a)
      } else {
        val fa = f(a)
        map.addOne(a, fa)
        fa
      }
    }
  }

}
