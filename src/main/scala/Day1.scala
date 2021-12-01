import scala.io.Source

object Day1 {

  def exercise1(input: List[Int]): Int = {
    input.sliding(2).map {
      case List(a, b) => a < b
      case List(_) => false
    }.count(_ equals true)
  }


  def main(args: Array[String]): Unit = {
    val inputArray = Source.fromResource("day1.txt").getLines.toList.map(_.toInt)
    println(exercise1(inputArray))
  }
}
