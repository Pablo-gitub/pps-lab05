package polyglot.a01a

import polyglot.a01a.Logics
import polyglot.a01a.Logics.Result
import polyglot.a05b.Pair
import util.Sequences.Sequence
import util.Sequences.Sequence.*

import scala.annotation.tailrec

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private val maxFailures: Int = 5
  private var hit: Sequence[Pair[Int, Int]] = Nil()
  private var failures = 0
  private val random = new scala.util.Random()
  private val boatRow: Int = random.nextInt(size)
  private val boatLeftCol: Int = random.nextInt(size - boat + 1)

  private def sizeSequence[X](seq: Sequence[X]): Int = {
    @tailrec
    def counterElements[Y](seq: Sequence[Y], count: Int): Int = seq match
      case Nil() => count
      case Cons(h, t) => counterElements(t, count + 1)

    counterElements(seq, 0)
  }

  override def hit(row: Int, col: Int): Result = {
    val isHit: Boolean = row == this.boatRow && col >= this.boatLeftCol && col < this.boatLeftCol + this.boat
    val pos = Pair(row, col)
    isHit match
      case true if !hit.contains(Pair(row, col)) =>
          hit = Cons(Pair(row, col), hit)
          if sizeSequence(hit) == this.boat then
            Result.WON
          else
            Result.HIT
      case false =>
        failures += 1
        if failures >= maxFailures then
          Result.LOST
        else
          Result.MISS
      case _ => Result.MISS
  }
