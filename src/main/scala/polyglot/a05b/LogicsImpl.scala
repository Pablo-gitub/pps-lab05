package polyglot.a05b

trait Pair[X, Y]:
  def x: X
  def y: Y
  def equals(other: Pair[X,Y]): Boolean

object Pair:
  def apply[X, Y](x: X, y: Y): Pair[X, Y] = PairImpl(x, y)

  private case class PairImpl[X, Y](x: X, y: Y) extends Pair[X, Y]:
    override def equals(other: Pair[X, Y]): Boolean = other.x == x && other.y == y

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val random = new scala.util.Random()

  private var tickCount: Int = 0

  private val initial: Pair[Int, Int] = Pair(random.between(1, size - 1), random.between(1, size - 1))

  override def tick(): Unit = tickCount += 1

  override def isOver: Boolean =
    initial.y - tickCount < 0 || initial.y + tickCount >= this.size ||
    initial.x - tickCount < 0 || initial.x + tickCount >= this.size;

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initial.x && (y-initial.y).abs <= tickCount) ||
    (y == initial.y && (x-initial.x).abs <= tickCount) ||
    (x-y == initial.x-initial.y && (x-initial.x).abs <= tickCount) ||
    (x+y == initial.x+initial.y && (x-initial.x).abs <= tickCount)
