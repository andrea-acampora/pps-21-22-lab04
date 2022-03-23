package u04lab.polyglot.a01b

import scala.util.Random.nextInt

import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.List
import u04lab.code.Stream

import u04lab.code.Option.{Some, None}
import u04lab.code.Stream.{take, toList, iterate}
import u04lab.code.List.{flatMap, filter, contains, append, length, Cons, Nil}

case class Pair[A](x: A, y: A)

class LogicsImpl(private val size: Int, private val numberOfMines: Int) extends Logics:

  private val _minesStream: Stream[Pair[Int]] = Stream.iterate(Pair(nextInt(size), nextInt(size)))(mine => {
    var newMine: Pair[Int] = Pair( nextInt(size) , nextInt(size))
    while (mine eq newMine) newMine = Pair(nextInt(size), nextInt(size)) ; newMine })

  private val _mines: List[Pair[Int]] = toList(take(_minesStream)(numberOfMines))
  private var _selected: List[Pair[Int]] = Nil()

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if contains(_mines, Pair(x, y)) then OptionToOptional(None())
    else
      _selected = append(_selected, Cons(Pair(x, y), Nil()))
      OptionToOptional(Some(minesInNeighbours(Pair(x, y))))

  def won: Boolean = length(_selected) + numberOfMines == size * size

  private def sameAxisNeighbours(axis: Int): List[Int] = toList(take(Stream.iterate(axis - 1)(_ + 1))(3))

  private def minesInNeighbours(value: Pair[Int]): Int =
    val allNeighbours: List[Pair[Int]] = flatMap(sameAxisNeighbours(value.x))(x => flatMap(sameAxisNeighbours(value.y))(y => Cons(Pair(x, y), Nil())))
    length(filter(allNeighbours)(contains(_mines, _)))