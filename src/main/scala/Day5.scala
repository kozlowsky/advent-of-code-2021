import scala.io.Source

object Day5 {

  def exercise1(points: List[((Int, Int), (Int, Int))]): Int = {
    val dimension = Math.round(points.flatMap(e => List(e._1._1, e._1._2, e._2._1, e._2._2)).max.ceil / 10) * 10

    def generatePoints(points: ((Int, Int), (Int, Int)), nth: Int = 1000): List[(Int, Int)] = {
      val smallerPoint = if (points._1._1 * nth + points._1._2 < points._2._1 * nth + points._2._2) points._1 else points._2
      if(points._1._1 == points._2._1) {
        (0 to Math.abs(points._1._2 - points._2._2)).map(e => (smallerPoint._1, smallerPoint._2 + e)).toList
      } else {
        (0 to Math.abs(points._1._1 - points._2._1)).map(e => (smallerPoint._1 + e, smallerPoint._2)).toList
      }
    }

    def fillDiagram(points: List[((Int, Int), (Int, Int))], diagram: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)): Int = {
      points.map(generatePoints(_, dimension)).foreach(e => e.foreach(f => diagram(f._1)(f._2) += 1))

      diagram.map(e => e.count(_ > 1)).sum
    }

    fillDiagram(points.filter(e => e._1._1 == e._2._1 || e._1._2 == e._2._2))
  }

  def main(args: Array[String]): Unit = {
    val input = readFile(args(0))
    println(exercise1(input))
  }

  private def readFile(fileName: String): List[((Int, Int), (Int, Int))] = {
    Source.fromResource(fileName).getLines()
      .map(_.replace(" -> ", " ").replace(",", " ").split(" "))
      .map(e => e.map(_.toInt))
      .map(line => ((line(0), line(1)), (line(2), line(3))))
      .toList
  }
}
