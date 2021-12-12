import scala.io.Source

object Day5 {

  def exercise1(points: List[((Int, Int), (Int, Int))]): Int = {
    val dimension = Math.round(points.flatMap(e => List(e._1._1, e._1._2, e._2._1, e._2._2)).max.ceil / 10) * 10

    def markPoints(points: ((Int, Int), (Int, Int)), diagram: Array[Array[Int]]): Unit = {
      if (points._1._1 == points._2._1) {
        (points._1._2 to points._2._2).foreach(e => diagram(e)(points._1._1) += 1)
      } else {
        (points._1._1 to points._2._1).foreach(e => diagram(points._1._2)(e) += 1)
      }
    }

    def fillDiagramAndGetSumOfOverlappingLines(points: List[((Int, Int), (Int, Int))], diagram: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)): Int = {
      points.foreach(e => markPoints(e, diagram))

      diagram.map(e => e.count(_ > 1)).sum
    }

    fillDiagramAndGetSumOfOverlappingLines(points.filter(e => e._1._1 == e._2._1 || e._1._2 == e._2._2))
  }

  def main(args: Array[String]): Unit = {
    val input = readFile(args(0))
    println(exercise1(sortEachPair(input)))
  }

  private def sortEachPair(input: List[((Int, Int), (Int, Int))]) = {
    input.map(e => {
      if (e._1._1 + e._1._2 > e._2._1 + e._2._2) (e._2, e._1)
      else (e._1, e._2)
    })
  }

  private def readFile(fileName: String): List[((Int, Int), (Int, Int))] = {
    Source.fromResource(fileName).getLines()
      .map(_.replace(" -> ", " ").replace(",", " ").split(" "))
      .map(e => e.map(_.toInt))
      .map(line => ((line(0), line(1)), (line(2), line(3))))
      .toList
  }
}
