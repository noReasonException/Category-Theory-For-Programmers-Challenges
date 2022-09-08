package ch2

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

class Task1Spec extends Specification {
  override def is: SpecStructure = sequential ^
    s2"""
      Memoized should run faster than nom Memoized func $memoizedSpec
      """

  import Task1._

  def time[A](f: => A): Long = {
    val initial: Long = System.currentTimeMillis()
    f
    System.currentTimeMillis() - initial
  }

  def memoizedSpec: MatchResult[Any] = {
    val nonMemoized: Int => Int = slowSquare
    val memoized: Int => Int    = memoize(nonMemoized)

    val nonMemoizedLatency = time((1 to 2).map(_ => nonMemoized(1)))
    val memoizedLatency    = time((1 to 2).map(_ => memoized(1)))

    nonMemoizedLatency should be_>(memoizedLatency)
  }

}
