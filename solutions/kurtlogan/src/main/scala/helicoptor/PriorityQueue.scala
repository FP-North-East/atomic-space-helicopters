package helicoptor

import cats.Eq
import cats.implicits._
import zio.{Ref, UIO}

final case class PriorityQueue[A: Ordering: Eq](private val nodes: Ref[List[A]]) {

  import Ordering.Implicits._

  def insert(node: A): UIO[Unit] =
    nodes.update { as =>
      val found = as.find(_ === node)

      found.fold(node :: as) {
        case n if n > node => node :: as.filterNot(_ === n) // keep the smallest
        case _             => as
      }
    }.unit

  def pop: UIO[Option[A]] =
    nodes.modify { as =>
      val found = as.sorted.headOption

      found.fold((found, as))(n => (found, as.filterNot(_ === n)))
    }

  def peek: UIO[Option[A]] =
    nodes.get.map(_.sorted.headOption)

  def toList: UIO[List[A]] =
    nodes.get

  def clear: UIO[Unit] = nodes.set(Nil)

  def isEmpty: UIO[Boolean] = nodes.get.map(_.isEmpty)

  def size: UIO[Int] = nodes.get.map(_.size)
}

object PriorityQueue {

  def make[A: Ordering: Eq](node: A): UIO[PriorityQueue[A]] =
    Ref.make(List(node)).map(PriorityQueue(_))

  def make[A: Ordering: Eq](): UIO[PriorityQueue[A]] =
    Ref.make[List[A]](Nil).map(PriorityQueue(_))
}