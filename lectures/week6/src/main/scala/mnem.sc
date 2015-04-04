
import scala.io._

object mnem {

  val in = Source.fromURL("i'll add this later")

  val words = in.getLines

  def mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ" )

  val charCode
}