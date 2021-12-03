object Day3 {

  def exercise1(input: List[String]): Int = {
    input
      .transpose
      .map(_.groupBy(identity).maxBy(_._2.size)).map(e => List(e._1, if (e._1 == '0') '1' else '0'))
      .transpose
      .map(e => Integer.parseInt(e.mkString, 2))
      .foldLeft(1)(_ * _)
  }

  def main(args: Array[String]): Unit = {
    println(exercise1(args.toList))

  }
}
