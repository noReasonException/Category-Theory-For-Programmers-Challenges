package ch2

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

import scala.util.Random

class Task3Spec extends Specification {
  override def is: SpecStructure = sequential ^
    s2"""
      Memoized random integer generator with seed should behave exacly like the non-memoized one(pure function)$memoizedSpec
      """

  import Task3._

  def memoizedSpec: MatchResult[Any] = {
    val nonMemoized: Int => Int = new Random(_).nextInt()
    val memoized: Int => Int    = memoize(nonMemoized)

    val nonMemoizedResult = (1 to 10).map(nonMemoized)
    val memoizedResult    = (1 to 10).map(memoized)

    // nonMemoized always returns different numbers, memoized always return same number, the first generated number
    nonMemoizedResult shouldEqual memoizedResult
  }

}
