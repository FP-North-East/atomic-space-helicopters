package helicoptor.utils

object ConsoleColours {

  val reset = "\u001B[0m";

  object text {

    val black = "\u001B[30m"
    val red = "\u001B[31m"
    val green = "\u001B[32m"
    val yellow = "\u001B[33m"
    val blue = "\u001B[34m"
    val purple = "\u001B[35m"
    val cyan = "\u001B[36m"
    val white = "\u001B[37m"
  }

  object background {

    val black = "\u001B[40m"
    val red = "\u001B[41m"
    val green = "\u001B[42m"
    val yellow = "\u001B[43m"
    val blue = "\u001B[44m"
    val purple = "\u001B[45m"
    val cyan = "\u001B[46m"
    val white = "\u001B[47m"
  }

  def format(colour: String, background: String, text: String): String =
    background + colour + text + reset

  def format(colour: String, text: String): String =
    colour + text + reset
}