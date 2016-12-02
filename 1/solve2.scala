import scala.collection.mutable.Set

object Main2 extends App {

  val input = "R5, R4, R2, L3, R1, R1, L4, L5, R3, L1, L1, R4, L2, R1, R4, R4, L2, L2, R4, L4, R1, R3, L3, L1, L2, R1, R5, L5, L1, L1, R3, R5, L1, R4, L5, R5, R1, L185, R4, L1, R51, R3, L2, R78, R1, L4, R188, R1, L5, R5, R2, R3, L5, R3, R4, L1, R2, R2, L4, L4, L5, R5, R4, L4, R2, L5, R2, L1, L4, R4, L4, R2, L3, L4, R2, L3, R3, R2, L2, L3, R4, R3, R1, L4, L2, L5, R4, R4, L1, R1, L5, L1, R3, R1, L2, R1, R1, R3, L4, L1, L3, R2, R4, R2, L2, R1, L5, R3, L3, R3, L1, R4, L3, L3, R4, L2, L1, L3, R2, R3, L2, L1, R4, L3, L5, L2, L4, R1, L4, L4, R3, R5, L4, L1, L1, R4, L2, R5, R1, R1, R2, R1, R5, L1, L3, L5, R2"

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction

  type Vector = (Int, Int) // x then y
  type Position = (Int, Int) // x then y

  val north: Vector = (1 , 0)
  val east:  Vector = (0 , 1)
  val south: Vector = (-1, 0)
  val west:  Vector = (0 ,-1)

  def right(v: Vector): Vector = {val (x, y) = v; (0-y, x  ) }
  def left(v: Vector) : Vector = {val (x, y) = v; (y  , 0-x) }

  case class State(p: Position, v: Vector)

  implicit class PositionOps(p: Position) {
    def +(v: Vector): Position = {
      (p._1 + v._1, p._2 + v._2)
    }
  }

  implicit class VectorOps(v: Vector) {
    def *(i: Int): Vector = {
      (v._1 * i, v._2 * i)
    }
  }

  val cache = Set.empty[Position]

  val result = input.split(",").toList.map(_.trim).foldLeft(State((0, 0), north)) { (s: State, instruction: String) =>
    val (direction :: countarray) = instruction.toList
    val count = countarray.mkString.toInt
    val vector = direction match {
      case 'R' => right(s.v)
      case 'L' => left(s.v)
    }
    val position = s.p + (vector * count)
    // add locations to cache
    // check if we've been here twice
    (1 to count).map(c => s.p + (vector * c)).foreach { p =>
      if (cache.contains(p)) {
        println(s"we've been here twice! $p")
      } else {
        cache.add(p)
      }
    }
    State(position, vector)
  }
  println(result)
  println(result.p._1.abs + result.p._2.abs)
}
