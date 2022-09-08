package ch2

import scala.util.Random

object Task5 {
  val truth: Boolean => Boolean       = x => true
  val falsity: Boolean => Boolean     = x => false
  val nothingness: Boolean => Boolean = x => { while (true) 1 + 1; true }
  val identity: Boolean => Boolean    = x => x
  val opposite: Boolean => Boolean    = x => !x
}
