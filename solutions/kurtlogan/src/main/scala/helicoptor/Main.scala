package helicoptor

import helicoptor.modules.{Logger, LoggerDebug, LoggerLive, Printer, PrinterDebug, PrinterLive, StepDebug, StepLive}
import io.circe.parser._
import zio.{IO, ZIO}
import zio.console.{Console, getStrLn}

import scala.io.{Source, StdIn}

object DebugMain extends Main {

  object Env extends StepDebug with LoggerDebug with PrinterDebug with Console.Live

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    program.provide(Env).fold(_ => 1, _ => 0)
}

object LiveMain extends Main {

  object Env extends StepLive with LoggerLive with PrinterLive with Console.Live

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    program.provide(Env).fold(_ => 1, _ => 0)
}

trait Main extends zio.App {

  import AStar._
  import Logger._
  import Node._
  import Printer._

  val resource = IO.effect(Source.fromResource("field1.json").getLines().toList)

  def readPosition(message: String) =
    (info(message) *> getStrLn)
      .flatMap(s => ZIO.fromOption(Pos.fromString(s)))
      .flatMapError(_ => info("Unable to read position. Enter 2 numbers separated by a space"))
      .eventually

  val program =
    for {
      lines  <- resource
      source <- readPosition("Enter start position:")
      dest   <- readPosition("Enter destination position:")
      result <- decode[Vector[Vector[String]]](lines.mkString).map { decoded =>

        search(make(source, decoded), dest, decoded).foldM(
          _       => error(s"Failed to find a path to $dest"),
          success => print(success, decoded)
        )

      }.getOrElse(error("Something went wrong") *> IO.fail(()))
    } yield result
}
