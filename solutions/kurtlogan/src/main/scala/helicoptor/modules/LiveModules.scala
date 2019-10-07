package helicoptor.modules
import helicoptor.Node
import zio.{IO, UIO, ZIO}
import zio.console.{Console, putStrLn}

trait StepLive extends Step {

  val step = new Step.Service { override def next: UIO[Unit] = IO.unit }
}

object StepLive extends StepLive


trait LoggerLive extends Logger {

  val logger = new Logger.Service {
    override def info(log: String): UIO[Unit] = putStrLn(log).provide(Console.Live)
    override def infoM(log: UIO[String]): UIO[Unit] = log.flatMap(putStrLn(_).provide(Console.Live))
    override def debug(log: String): UIO[Unit] = IO.unit
    override def debugM(log: UIO[String]): UIO[Unit] = IO.unit
    override def error(log: String): UIO[Unit] = putStrLn(log).provide(Console.Live)
  }
}

object LoggerLive extends LoggerLive


trait PrinterLive extends Printer {

  val printer = (_: Node, _: List[Node], _: List[Node], _: List[Node], _: Vector[Vector[String]]) => IO.unit
}

object PrinterLive extends PrinterLive