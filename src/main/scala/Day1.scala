import scala.io.Source

object Day1 {

  def exercise1(input: List[Int]): Int = {
    input.sliding(2).map {
      case List(a, b) => a < b
      case List(_) => false
    }.count(_ equals true)
  }

  def exercise2(input: List[Int]): Int = {
    exercise1(input.sliding(3).map {
      case List(a, b, c) => a + b + c
      case List(_,_) => 0
      case List(_) => 0
    }.toList)
  }


  def main(args: Array[String]): Unit = {
    val inputArray = args.map(_.toInt).toList
    println(exercise1(inputArray))
    println(exercise2(inputArray))
  }
}
