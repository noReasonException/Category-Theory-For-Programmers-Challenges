package ch2

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

import scala.util.{ Random, Try }

class Task5Spec extends Specification {
  override def is: SpecStructure = sequential ^
    s2"""
      truth $truthSpec
      false
      nothingness
      identity
      opposite
      """

  import Task5._

  def truthSpec: MatchResult[Any] =
    (truth(false) shouldEqual true) and (truth(true) shouldEqual true)
  def falsitySpec: MatchResult[Any] =
    (falsity(false) shouldEqual false) and (falsity(true) shouldEqual false)
  def nothingnessSpec: MatchResult[Any] =
    (Try(nothingness(false)).isFailure shouldEqual true) and (Try(nothingness(true)).isFailure shouldEqual true)
  def identitySpec: MatchResult[Any] =
    (identity(true) shouldEqual true) and (identity(false) shouldEqual false)
  def oppositeSpec: MatchResult[Any] =
    (opposite(true) shouldEqual false) and (opposite(false) shouldEqual true)

}
