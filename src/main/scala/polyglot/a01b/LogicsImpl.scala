package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import polyglot.a05b.Pair
import util.Sequences.Sequence
import util.Sequences.Sequence.*

import scala.annotation.tailrec
import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var mineSet: Sequence[Pair[Int, Int]] = Nil()
  private var selected: Sequence[Pair[Int, Int]] = Nil()
  private val random = new scala.util.Random()

  setMines(mines)

  @tailrec
  private def setMines(mines: Int): Unit = {
    if (mines > 0) {
      val newMine = Pair(random.between(1, size - 1), random.between(1, size - 1))

      if (!mineSet.contains(newMine)) {
        mineSet = Cons(newMine, mineSet)
        setMines(mines - 1)
      } else {
        setMines(mines)
      }
    }
  }

  private def neighbours(pos: Pair[Int,Int]): Int = {
    val neighbourPositions = generateNeighbours(pos)
    neighbourPositions.count(p => mineSet.contains(p))
  }

  private def generateNeighbours(pos: Pair[Int,Int]): Seq[Pair[Int, Int]] = {
    for {
      x <- (pos.x - 1) to (pos.x + 1)
      y <- (pos.y - 1) to (pos.y + 1)
    } yield Pair(x, y)
  }

  private def sizeSequence[X](seq: Sequence[X]): Int = {
    @tailrec
    def counterElements[Y](seq: Sequence[Y], count: Int): Int = seq match
      case Nil() => count
      case Cons(h, t) => counterElements(t, count + 1)

    counterElements(seq, 0)
  }

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val pos = Pair(x, y)
    pos match
      case p if mineSet.contains(p) => OptionToOptional(ScalaOptional.Empty())
      case p =>
        if !selected.contains(p) then selected = Cons(p, selected)
        OptionToOptional(ScalaOptional.Just(neighbours(p)))

  override def won: Boolean = sizeSequence(selected) == this.size * this.size - sizeSequence(mineSet)
