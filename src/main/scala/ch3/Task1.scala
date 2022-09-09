package ch3

import scala.util.Try

object Writer {

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
          case None => None.liftToWriter
        }
        resB
      }
  }
  abstract class MyMonoid[A] {
    def combine(a: A)(b: A): A
    def id: A
  }
  class MyOptionMonoid[A](implicit aMonoid: MyMonoid[A]) extends MyMonoid[Option[A]] {
    override def combine(a: Option[A])(b: Option[A]): Option[A] = {
      (a, b) match {
        case (Some(v1), Some(v2)) => Some(aMonoid.combine(v1)(v2))
        case (Some(v1), None)     => Some(v1)
        case (None, Some(v2))     => Some(v2)
        case (None, None)         => None
      }
    }
    override def id: Option[A] = None
  }

  abstract class MyMonad[F[_]] {
    def map[A, B](v: F[A])(fn: A => B): F[B]
    def flatMap[A, B](v: F[A])(fn: A => F[B]): F[B]
    def pure[A](a: A): F[A]
  }
  class MyWriterMonad extends MyMonad[Writer] {
    override def map[A, B](v: Writer[A])(fn: A => B): (B, String) = flatMap(v)(fn andThen pure)
    override def flatMap[A, B](v: Writer[A])(fn: A => Writer[B]): Writer[B] = {
      val logA = v._2
      val opB  = fn(v._1)
      (opB._1, logA + opB._2)
    }
    override def pure[A](a: A): Writer[A] = a.liftToWriter
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
    Try
      .apply(Math.sqrt(x))
      .toOption
      .liftToWriter
      .withLogIfSuccess("Safe Root")
      .withLogIfFailure("Safe Root of 0 Undefined")
  def safe_reciprocal(x: Double): Writer[Option[Double]] =
    Try
      .apply(1 / x)
      .toOption
      .liftToWriter
      .withLogIfSuccess("Safe Reciprocal")
      .withLogIfFailure("Safe Reciprocal of 0 Undefined")

  def main(args: Array[String]): Unit = {
    // Test 1
    val writerA: Double => Writer[Int] = _ => 42.liftToWriter.withLog("I am 42")
    val writerB: Int => Writer[Double] = _ => 42d.liftToWriter.withLog("I am 42 Double")
    // println((writerA >=> writerB)(42))

    val safe_root_reciprocal: Double => Writer[Option[Double]] = safe_reciprocal _ >=> safe_root _
    println(safe_root_reciprocal(32d))

  }
}
