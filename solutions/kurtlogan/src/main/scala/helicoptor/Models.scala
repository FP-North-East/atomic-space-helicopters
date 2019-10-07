package helicoptor
import cats.Eq

import scala.util.Try
import cats.implicits._

final case class Pos(x: Int, y: Int) {

  def north: Pos = Pos(x, y - 1)
  def east: Pos = Pos(x + 1, y)
  def south: Pos = Pos(x, y + 1)
  def west: Pos = Pos(x - 1, y)
}

object Pos {

  def fromString(s: String): Option[Pos] = {
    s.split(Array(',', ':', '.', ' ')).toList match {
      case x :: y :: Nil => (Try(x.trim.toInt).toOption, Try(y.trim.toInt).toOption).mapN(Pos.tupled)
      case _             => None
    }
  }

  val tupled: (Int, Int) => Pos = {
    case (x, y) => Pos(x, y)
  }

  def origin: Pos = Pos(0, 0)
}

sealed trait Node
final case class Space(pos: Pos, north: () => Node, east: () => Node, south: () => Node, west: () => Node) extends Node {

  override def toString: String = pos.toString

  override def equals(o: Any): Boolean = o match {
    case Space(oPos, _, _, _, _) => canEqual(o) && oPos == pos
    case _                       => false
  }

  override def hashCode(): Int = pos.hashCode()

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Space]
}

case object Block extends Node
case object Edge extends Node

object Node {

  def space(pos: Pos, north: => Node, east: => Node, south: => Node, west: => Node): Node =
    Space(pos, () => north, () => east, () => south, () => west)

  def edge: Node = Edge

  def block: Node = Block

  def make(start: Pos, map: Vector[Vector[String]]): Node = {

    def getNode(pos: Pos): Node = {
      map.lift(pos.y).flatMap(_.lift(pos.x)).collect {
        case " " => space(pos, getNode(pos.north), getNode(pos.east), getNode(pos.south), getNode(pos.west))
        case "#" => block
      }.getOrElse(edge)
    }

    getNode(start)
  }
}

final case class PathPoint(node: Node, path: List[Node], manDist: Int) {

  def h: Int = (path.size * 2) + (manDist * 3)
}

object PathPoint {

  def make(node: Node, dist: Int): PathPoint = PathPoint(node, Nil, dist)

  implicit val eq: Eq[PathPoint] =
    (x: PathPoint, y: PathPoint) => x.node == y.node

  implicit val ordering: Ordering[PathPoint] =
    (x: PathPoint, y: PathPoint) => x.h - y.h match {
      case 0 => x.manDist - y.manDist
      case n => n
    }
}
