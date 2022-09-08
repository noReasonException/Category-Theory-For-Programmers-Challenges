package ch2

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

import scala.util.Random

class Task2Spec extends Specification {
  override def is: SpecStructure = sequential ^
    s2"""
      Memoized random integer generator without seed should behave differently from non-memoized one(non-pure function)$memoizedSpec
      """

  import Task2._

  def memoizedSpec: MatchResult[Any] = {
    val random: scala.util.Random = new Random(12)
    val nonMemoized: Unit => Int  = _ => random.nextInt()
    val memoized: Unit => Int     = memoize(nonMemoized)

    val nonMemoizedResult = (1 to 10).map(_ => nonMemoized())
    val memoizedResult    = (1 to 10).map(_ => memoized())

    // nonMemoized always returns different numbers, memoized always return same number, the first generated number
    (nonMemoizedResult shouldNotEqual memoizedResult) and (
      memoizedResult.forall(_ == memoizedResult.head) shouldEqual true
    )
  }

}
