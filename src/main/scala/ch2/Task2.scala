package ch2

import scala.util.Random

object Task2 {

  // Try to memoize a function from your standard library that you normally use to produce random numbers. Does it work?
  // It wont work, as Random(_).nextInt() is based on state(performs side effects), the state is needed as every .nextInt()
  // uses the previous random number as a seed
  // to calculate the next integer in line.
  def main(args: Array[String]): Unit = {
    val random: scala.util.Random = new Random(12)
    val nonMemoized: Unit => Int  = _ => random.nextInt()
    val memoized: Unit => Int     = memoize(nonMemoized)

    val nonMemoizedResult = (1 to 10).map(_ => nonMemoized())
    val memoizedResult    = (1 to 10).map(_ => memoized())

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
