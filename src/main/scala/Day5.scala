import scala.annotation.tailrec
import scala.io.Source

object Day5 {

  def exercise1(points: List[((Int, Int), (Int, Int))]): Int = {
    val dimension = calculateDimension(points.flatMap(e => List(e._1._1, e._1._2, e._2._1, e._2._2)).max)

    fillDiagramAndGetSumOfOverlappingLines(
      sortEachPair(points).filter(e => e._1._1 == e._2._1 || e._1._2 == e._2._2),
      Array.ofDim[Int](dimension, dimension))
  }

  def exercise2(points: List[((Int, Int), (Int, Int))]): Int = {
    val dimension = calculateDimension(points.flatMap(e => List(e._1._1, e._1._2, e._2._1, e._2._2)).max)

    fillDiagramAndGetSumOfOverlappingLines(
      sortEachPair(points),
      Array.ofDim[Int](dimension, dimension))
  }

  private def fillDiagramAndGetSumOfOverlappingLines(points: List[((Int, Int), (Int, Int))], diagram: Array[Array[Int]] = Array.ofDim[Int](10, 10)): Int = {
    points.flatMap(getMarkingLine).foreach(point => diagram(point._2)(point._1) += 1)

    diagram.map(e => e.count(_ > 1)).sum
  }

  private def getMarkingLine(points: ((Int, Int), (Int, Int))): List[(Int, Int)] = {
    if (points._1._1 == points._2._1) {
      (points._1._2 to points._2._2).map((points._1._1, _)).toList
    } else if (points._1._2 == points._2._2) {
      (points._1._1 to points._2._1).map((_, points._1._2)).toList
    } else if ((points._1._1 + points._1._2) - (points._2._1 + points._2._2) < 0) {
      (0 to Math.abs(points._1._1 - points._2._1)).map(e => (points._1._1 + e, points._1._2 + e)).toList
    } else {
      (0 to Math.abs(points._1._1 - points._2._1)).map(e => (points._1._1 + e, points._1._2 - e)).toList
    }
  }

  @tailrec
  private def calculateDimension(maxValue: Int, currentDim: Int = 10): Int = {
    if (maxValue > currentDim) calculateDimension(maxValue, currentDim * 10)
    else currentDim
  }


  def main(args: Array[String]): Unit = {
    val input = readFile(args(0))
    println(exercise1(input))
    println(exercise2(input))
  }

  private def sortEachPair(input: List[((Int, Int), (Int, Int))]) = {
    input.map(e => {
      if (e._1._1 * input.length + e._1._2 >= e._2._1 * input.length + e._2._2) (e._2, e._1)
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
