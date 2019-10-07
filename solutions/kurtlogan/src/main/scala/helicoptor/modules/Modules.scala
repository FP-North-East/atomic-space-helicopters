package helicoptor.modules

import com.softwaremill.quicklens._
import helicoptor.modules.Logger.info
import helicoptor.{Node, Pos, Space}
import helicoptor.utils.ConsoleColours.{background, format, text}
import zio.{UIO, ZIO}

trait Step {
  val step: Step.Service
}

object Step {

  trait Service {
    def next: UIO[Unit]
  }

  def next: ZIO[Step, Nothing, Unit] =
    ZIO.accessM(_.step.next)
}

trait Logger {

  val logger: Logger.Service
}

object Logger {

  trait Service {
    def info(log: String): UIO[Unit]
    def infoM(log: UIO[String]): UIO[Unit]
    def debug(log: String): UIO[Unit]
    def debugM(log: UIO[String]): UIO[Unit]
    def error(log: String): UIO[Unit]
  }

  def info(log: String): ZIO[Logger, Nothing, Unit] =
    ZIO.accessM(_.logger.info(log))

  def infoM(log: UIO[String]): ZIO[Logger, Nothing, Unit] =
    ZIO.accessM(_.logger.infoM(log))

  def debug(log: String): ZIO[Logger, Nothing, Unit] =
    ZIO.accessM(_.logger.debug(log))

  def debugM(log: UIO[String]): ZIO[Logger, Nothing, Unit] =
    ZIO.accessM(_.logger.debugM(log))

  def error(log: String): ZIO[Logger, Nothing, Unit] =
    ZIO.accessM(_.logger.error(log))
}

trait Printer {

  val printer: Printer.Service
}

object Printer {

  trait Service {

    def fullPrint(current: Node, path: List[Node], inQueue: List[Node], visited: List[Node], map: Vector[Vector[String]]): ZIO[Logger, Nothing, Unit]

    def print(path: List[Node], map: Vector[Vector[String]]): ZIO[Logger, Nothing, Unit] =
      ZIO.traverse_(
        formatPoint(path, format(text.red, background.black, "x"))(map.map(_.map(s => format(text.black, background.white, if (s == " ") "." else s)))))(x => info(x.mkString))

    protected def formatPoint(path: List[Node], format: String): Vector[Vector[String]] => Vector[Vector[String]] =
      path.foldLeft(_) { (map, curr) =>
        curr match {
          case Space(Pos(x, y), _, _, _, _) => map.modify(_.at(y).at(x)).setTo(format)
          case _                            => map
        }
      }
  }

  def print(path: List[Node], map: Vector[Vector[String]]): ZIO[Printer with Logger, Nothing, Unit] =
    ZIO.accessM(_.printer.print(path, map))

  def fullPrint(current: Node, path: List[Node], inQueue: List[Node], visited: List[Node], map: Vector[Vector[String]]): ZIO[Printer with Logger, Nothing, Unit] =
    ZIO.accessM(_.printer.fullPrint(current, path, inQueue, visited, map))
}