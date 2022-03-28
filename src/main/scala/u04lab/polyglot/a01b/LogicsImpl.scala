package u04lab.polyglot.a01b

import scala.util.Random.nextInt

import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.List
import u04lab.code.Stream

import u04lab.code.Option.{Some, None}
import u04lab.code.Stream.{take, toList, iterate}
import u04lab.code.List.{flatMap, filter, contains, append, length, Cons, Nil}

class LogicsImpl(private val size: Int, private val numberOfMines: Int) extends Logics:

  private val minesStream: Stream[(Int, Int)] = iterate(nextInt(size), nextInt(size))( _ => (nextInt(size), nextInt(size)))
  private val mines: List[(Int, Int)] = toList(take(minesStream)(numberOfMines))
  private var selected: List[(Int, Int)] = Nil()

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    println(mines)
    if contains(mines, (x, y)) then
      OptionToOptional(None())
    else
      selected = append(selected, Cons((x, y), Nil()))
      OptionToOptional(Some(minesInNeighbours((x, y))))

  def won: Boolean = length(selected) + numberOfMines == size * size

  private def sameAxisNeighbours(axis: Int): List[Int] = toList(take(Stream.iterate(axis - 1)(_ + 1))(3))

  private def minesInNeighbours(value: (Int, Int)): Int =
    val allNeighbours: List[(Int, Int)] = flatMap(sameAxisNeighbours(value._1 ))(x => flatMap(sameAxisNeighbours(value._2))(y => Cons((x, y), Nil())))
    length(filter(allNeighbours)(contains(mines, _)))
