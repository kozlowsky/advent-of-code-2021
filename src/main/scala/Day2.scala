import scala.io.Source

object Day2 {

  def exercise1(input: List[String]): Int = {
    val res = input.map(_.split(" ")).map(e => e(0) -> e(1).toInt).foldLeft(List(0, 0)) {
      case (acc, ("forward", i)) => List(acc(0), acc(1) + i)
      case (acc, ("up", i)) => List(acc(0) - i, acc(1))
      case (acc, ("down", i)) => List(acc(0) + i, acc(1))
    }
    res(0) * res(1)
  }

  def exercise2(input: List[String]): Int = {
    val res = input.map(_.split(" ")).map(e => e(0) -> e(1).toInt).foldLeft(List(0, 0, 0)) {
      case (acc, ("forward", i)) => List(acc(0) + acc(2) * i, acc(1) + i, acc(2))
      case (acc, ("up", i)) => List(acc(0), acc(1), acc(2) - i)
      case (acc, ("down", i)) => List(acc(0), acc(1), acc(2) + i)
    }

    res(0) * res(1)
  }

  def main(args: Array[String]): Unit = {
    val inputArray = Source.fromResource("day2.txt").getLines().toList
    println(exercise1(inputArray))
    println(exercise2(inputArray))
  }
}
