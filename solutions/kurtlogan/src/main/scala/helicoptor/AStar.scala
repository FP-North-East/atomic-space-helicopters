package helicoptor

import helicoptor.modules.{Logger, Printer, Step}
import zio.{IO, Ref, ZIO}

object AStar {

  type IOResult = ZIO[Logger with Printer with Step, Unit, List[Node]]

  def search(start: Node, dest: Pos, map: Vector[Vector[String]]): IOResult = {

    import Logger._
    import Step._
    import Printer._

    def doSearch(node: Node, queue: PriorityQueue[PathPoint], path: List[Node], visited: Ref[Set[Node]]): IOResult = node match {
      case Edge | Block                         => IO.fail(())
      case node@Space(_, north, east, south, west) =>

        def insert(node: Node): ZIO[Logger, Nothing, Unit] = node match {
          case node@Space(pos, _, _, _, _) =>
            info(s"inserting $pos") *>
            debugM(visited.get.map(!_.contains(node)).map(b => s"can insert ${b.toString}")) *>
            IO.whenM(visited.get.map(!_.contains(node))) { queue.insert(PathPoint(node, node :: path, distToDest(node, dest))) }
          case _ => IO.unit
        }

        def printMap(curr: Node, path: List[Node]): ZIO[Printer with Step with Logger, Nothing, Unit] =
          for {
            q <- queue.toList
            v <- visited.get
            _ <- fullPrint(curr, path, q.map(_.node), v.toList, map)
            _ <- next
          } yield {
            ()
          }

        for {
          _      <- visited.update(_ + node)
          _      <- insert(north())
          _      <- insert(east())
          _      <- insert(south())
          _      <- insert(west())
          result <- for {
            _     <- debug("queue") *> debugM(queue.toList.map(_.map(a => (a.node, a.h, a.manDist)).toString))
            _     <- debug("visited") *> debugM(visited.get.map(_.toString))
            top   <- queue.pop
            route <- top.fold[IOResult] {
              error("Queue is empty!") *> IO.fail(())
            } { top =>
               for {
                _     <- info(s"searching node: ${top.node}")
                route <-
                  if(found(top.node, dest))
                    infoM(visited.get.map(as => s"searched ${as.size} locations")) *> IO.succeed(top.path)
                  else
                    printMap(top.node, top.path) *> doSearch(top.node, queue, top.path, visited)
              } yield route
            }
          } yield route
        } yield result
    }

    for {
      visited <- Ref.make(Set[Node]())
      queue   <- PriorityQueue.make[PathPoint]()
      result  <- doSearch(start, queue, start :: Nil, visited)
    } yield result
  }

  private def distToDest(node: Node, dest: Pos): Int = node match {
    case Edge | Block           => Int.MaxValue
    case Space(pos, _, _, _, _) => Math.abs(pos.x - dest.x) + Math.abs(pos.y - dest.y)
  }

  private def found(node: Node, dest: Pos): Boolean = node match {
    case Edge | Block           => false
    case Space(pos, _, _, _, _) => pos == dest
  }
}