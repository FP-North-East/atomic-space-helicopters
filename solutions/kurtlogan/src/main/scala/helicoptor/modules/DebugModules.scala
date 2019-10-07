package helicoptor.modules

import helicoptor.Node
import helicoptor.modules.Logger.info
import helicoptor.utils.ConsoleColours.{background, format, text}
import zio.{IO, UIO, ZIO}
import zio.console.{Console, getStrLn, putStrLn}

trait StepDebug extends Step {

  val step = new Step.Service { override def next: UIO[Unit] =
    (putStrLn("Press enter to proceed!") *> getStrLn).eventually.unit.provide(Console.Live)
  }
}

object StepDebug extends StepDebug


trait LoggerDebug extends Logger {

  val logger = new Logger.Service {
    override def info(log: String): UIO[Unit] = putStrLn(log).provide(Console.Live)
    override def infoM(log: UIO[String]): UIO[Unit] = log.flatMap(putStrLn(_).provide(Console.Live))
    override def debug(log: String): UIO[Unit] = putStrLn(log).provide(Console.Live)
    override def debugM(log: UIO[String]): UIO[Unit] = log.flatMap(s => putStrLn(s).provide(Console.Live))
    override def error(log: String): UIO[Unit] = putStrLn(log).provide(Console.Live)
  }
}

object LoggerDebug extends LoggerDebug


trait PrinterDebug extends Printer {

  val printer = new Printer.Service {
    override def fullPrint(
      current: Node,
      path: List[Node],
      inQueue: List[Node],
      visited: List[Node],
      map: Vector[Vector[String]]
    ): ZIO[Logger, Nothing, Unit] = {

      val formatMap =
        formatPoint(visited, format(text.black, background.red, "v")) andThen
          formatPoint(inQueue, format(text.black, background.yellow, "q")) andThen
          formatPoint(path, format(text.black, background.blue, "x")) andThen
          formatPoint(List(current), format(text.black, background.green, "x"))

      ZIO.traverse_(formatMap(map.map(_.map(s => format(text.black, background.white, if (s == " ") "." else s)))))(x => info(x.mkString))
    }
  }
}

object PrinterDebug extends PrinterDebug