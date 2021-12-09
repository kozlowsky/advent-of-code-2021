import scala.annotation.tailrec
import scala.io.Source

object Day4 {


  def exercise1(bingoNumbers: List[Int], bingoTables: List[Array[Array[Int]]]): Int = {
    @tailrec
    def exercise1Helper(bingoNumbers: List[Int], bingoTables: List[Array[Array[Int]]], currentState: Array[Array[Array[Int]]]): Int = {
      if (bingoNumbers.length <= 0) return -1

      updateBingoTableState(bingoNumbers.head, bingoTables, currentState)

      val winningTableState = currentState.zipWithIndex.find(e => isTableWinning(e._1))
      if (winningTableState.isDefined) {
        return calculateWinningValue(bingoNumbers.head, bingoTables(winningTableState.get._2), winningTableState.get._1)
      }

      exercise1Helper(bingoNumbers.tail, bingoTables, currentState)
    }

    exercise1Helper(bingoNumbers, bingoTables, Array.ofDim[Int](bingoTables.length, bingoTables.head.length, bingoTables.head(0).length))
  }

  def exercise2(bingoNumbers: List[Int], bingoTables: List[Array[Array[Int]]]): Int = {

    @tailrec
    def exercise2Helper(bingoNumbers: List[Int],
                        bingoTables: List[Array[Array[Int]]],
                        currentState: Array[Array[Array[Int]]] = Array.ofDim[Int](bingoTables.length, bingoTables.head.length, bingoTables.head(0).length),
                        lastWinningTable: Array[Array[Int]] = Array(),
                        lastWinningTableState: Array[Array[Int]] = Array(),
                        lastWinningNumber: Int = -1): (Array[Array[Int]], Array[Array[Int]], Int) = {
      if (bingoNumbers.isEmpty || bingoTables.isEmpty) return (lastWinningTable, lastWinningTableState, lastWinningNumber)

      updateBingoTableState(bingoNumbers.head, bingoTables, currentState)

      val winningTableState = currentState.zipWithIndex.filter(e => isTableWinning(e._1))
      var (newWinningTable, newWinningTableState, newWinningBingoNumber, newCurrentState, newBingoTables) = (lastWinningTable, lastWinningTableState, lastWinningNumber, currentState, bingoTables)

      winningTableState.foreach(e => {
        newWinningTableState = e._1
        newWinningTable = bingoTables(e._2)
        newWinningBingoNumber = bingoNumbers.head

        newBingoTables = newBingoTables.filterNot(_ sameElements newWinningTable)
        newCurrentState = newCurrentState.filterNot(_ sameElements newWinningTableState)
      })

      exercise2Helper(bingoNumbers.tail, newBingoTables, newCurrentState, newWinningTable, newWinningTableState, newWinningBingoNumber)
    }


    val (lastWinningTable, lastWinningTableState, lastWinningNumber) = exercise2Helper(bingoNumbers, bingoTables)
    calculateWinningValue(lastWinningNumber, lastWinningTable, lastWinningTableState)
  }

  private def isTableWinning(bingoTable: Array[Array[Int]]): Boolean = {
    bingoTable.map(_.sum).contains(5) || bingoTable.transpose.map(_.sum).contains(5)
  }

  private def calculateWinningValue(currentNumber: Int, currentTable: Array[Array[Int]], currentTableState: Array[Array[Int]]): Int = {
    val sumOfNonPicked = currentTable.zipWithIndex.map(e => e._1.zipWithIndex.filter(f => currentTableState(e._2)(f._2) == 0).map(_._1).sum).sum

    currentNumber * sumOfNonPicked
  }

  def updateBingoTableState(currentBingoNumber: Int, bingoTables: List[Array[Array[Int]]], currentState: Array[Array[Array[Int]]]): Unit = {
    bingoTables.zipWithIndex.foreach(e => e._1.zipWithIndex
      .filter(f => f._1.contains(currentBingoNumber))
      .map(f => (f._1.indexOf(currentBingoNumber), f._2))
      .filter(f => f._1 > -1)
      .map(f => {
        (e._2, f._2, f._1)
      }).foreach(f => currentState(f._1)(f._2)(f._3) = 1))
  }

  def main(args: Array[String]): Unit = {
    val (bingoNumbers, bingoTables) = readFile(args(0))

    println(exercise1(bingoNumbers, bingoTables))
    println(exercise2(bingoNumbers, bingoTables))
  }

  private def readFile(path: String): (List[Int], List[Array[Array[Int]]]) = {
    val iterator = Source.fromResource(path).getLines()
    val bingoNumbers: List[Int] = iterator.next().split(",").toList.map(_.toInt)

    val bingoTables = iterator.toArray.filterNot(_.isBlank).flatMap(_.replace("  ", " ").trim.split(" ").map(_.toInt)).grouped(25).map(_.grouped(5).toArray).toList


    (bingoNumbers, bingoTables)
  }
}
