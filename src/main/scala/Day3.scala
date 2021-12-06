import scala.annotation.tailrec

object Day3 {

  def exercise1(input: List[String]): Int = {
    input
      .transpose
      .map(_.groupBy(identity).maxBy(_._2.size))
      .map(e => List(e._1, if (e._1 == '0') '1' else '0'))
      .transpose
      .map(e => Integer.parseInt(e.mkString, 2)).product
  }

  def exercise2(input: List[String]): Int = {
    @tailrec
    def exercise2Helper(calcFunc: (Int, List[String]) => Char, innerInput: List[String] = input, index: Int = 0): String = {
      if (index >= innerInput.head.length || innerInput.length == 1) {
        innerInput.head
      }
      else {
        val newBinary = calcFunc.apply(index, innerInput)
        exercise2Helper(calcFunc, innerInput.filter(_ (index) == newBinary), index + 1)
      }
    }

    val predicate: (Int, List[String]) => Boolean = (index, input) => input.map(_ (index)).count(_ == '1') >= (input.length.doubleValue / 2)

    val number1 = exercise2Helper((index, input) => if (predicate(index, input)) '1' else '0')
    val number2 = exercise2Helper((index, input) => if (predicate(index, input)) '0' else '1')

    Integer.parseInt(number1, 2) * Integer.parseInt(number2, 2)
  }


  def main(args: Array[String]): Unit = {
    println(exercise1(args.toList))
    println(exercise2(args.toList))
  }
}
