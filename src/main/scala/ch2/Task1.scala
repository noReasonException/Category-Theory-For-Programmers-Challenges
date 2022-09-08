package ch2

object Task1 {

  /**
   * Define a higher-order function (or a function object) memoize in your fa-vorite language. This function takes a
   * pure function f as an argument and returns a function that behaves almost exactly like f, except that it only calls
   * the original function once for every argument, stores the result in- ternally, and subsequently returns this stored
   * result every time it’s called with the same argument. You can tell the memoized function from the original by
   * watching its performance. For instance, try to memoize a func- tion that takes a long time to evaluate. You’ll have
   * to wait for the result the first time you call it, but on subsequent calls, with the same argument, you should get
   * the result immediately.
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val testF: Int => Int = slowSquare
    val memoized          = memoize(testF)

    val tenTimes = (1 to 10).map(_ => memoized(1))
    println(tenTimes)

  }
  def slowSquare(a: Int): Int = {
    Thread.sleep(1000)
    a * a
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
