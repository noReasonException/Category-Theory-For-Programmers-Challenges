package ch3

import scala.util.Try

object Task1 {

  // A is every object. Functions from Writer[A] => Writer[B] are morphisms
  type Writer[A] = (A, String) // basic structure to carry extra info

  // Compose writers

  implicit class ComposeWriterOps[A, B](left: A => Writer[B]) {
    def >=>[C](right: B => Writer[C]): A => Writer[C] = v => {
      val resA = left(v)
      val resB = right(resA._1)
      (resB._1, resA._2 + resB._2)
    }
  }
  // this is terrible, i should refactor this, the idea of combining Options can be factored out as Monoids
  implicit class ComposeWriterGenericOps[A, B](left: A => Writer[Option[B]]) {
    def >=>[C](right: B => Writer[Option[C]]): A => Writer[Option[C]] =
      v => {
        val resA = left(v)
        val resB = resA._1 match {
          case Some(value) =>
            val a = right(value)
            (a._1, resA._2 + "\n" + a._2)
          case None => None.liftToWriter.withLog(resA._2)
        }
        resB
      }
  }
  implicit class WriterLiftOp[A](a: A) {
    def liftToWriter: Writer[A] = (a, "")
  }
  implicit class WriterOps[A](writer: Writer[A]) {
    def withLog(s: String): Writer[A] = (writer._1, s)

  }
  implicit class WriterOptionOps[A](writer: Writer[Option[A]]) {
    def withLogIfSuccess(successLog: String): Writer[Option[A]] = writer._1 match {
      case Some(_) => writer.withLog(successLog)
      case None    => writer
    }
    def withLogIfFailure(failureLog: String): Writer[Option[A]] = writer._1 match {
      case Some(_) => writer
      case None    => writer.withLog(failureLog)
    }
  }

  def safe_root(x: Double): Writer[Option[Double]] =
    Option
      .when(x != 0d)(Math.sqrt(x))
      .liftToWriter
      .withLogIfSuccess("Safe Root")
      .withLogIfFailure("Safe Root of 0 Undefined")
  def safe_reciprocal(x: Double): Writer[Option[Double]] =
    Option
      .when(x != 0)(1 / x)
      .liftToWriter
      .withLogIfSuccess("Safe Reciprocal")
      .withLogIfFailure("Safe Reciprocal of 0 Undefined")

  def main(args: Array[String]): Unit = {
    // Test 1
    val writerA: Double => Writer[Int] = _ => 42.liftToWriter.withLog("I am 42")
    val writerB: Int => Writer[Double] = _ => 42d.liftToWriter.withLog("I am 42 Double")
    // println((writerA >=> writerB)(42))

    val safe_root_reciprocal: Double => Writer[Option[Double]] = safe_root _ >=> safe_reciprocal _
    println(safe_root_reciprocal(11d))

  }
}
