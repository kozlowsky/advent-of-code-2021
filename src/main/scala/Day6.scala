import scala.annotation.tailrec
import scala.io.Source

object Day6 {

  def exercise1(lanternfish: List[Int]): Int = {
    simulateLanternfishLife(lanternfish).size
  }

  @tailrec
  private def simulateLanternfishLife(lanternfish: List[Int], limit: Int = 80): List[Int] = {
    if(limit == 0) lanternfish
    else {
      val newFishCount = lanternfish.count(_ == 0)
      val resetDays = lanternfish.map(e => if (e == 0) 7 else e)
      val nextDay = resetDays.map(_ - 1)
      simulateLanternfishLife(nextDay :++ List.fill(newFishCount)(8), limit - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource(args(0)).getLines().flatMap(_.split(",")).map(_.toInt).toList

    println(exercise1(input))
  }
}
