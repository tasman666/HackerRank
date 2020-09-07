import java.io.PrintWriter

object ClimbingLeaderboard {

  // Complete the climbingLeaderboard function below.
    def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {
      if (invalidArray(scores) || invalidArray(alice)) {
        throw new IllegalArgumentException("Wrong input")
      }

      climbingLeaderboard(scores, alice, Array())
    }

    private def invalidArray(array: Array[Int]) = {
      array.length < 1 || array.length > 2 * Math.pow(10, 5)
    }

    @scala.annotation.tailrec
    private def climbingLeaderboard(scores: Array[Int], alice: Array[Int], result: Array[Int]): Array[Int] = {
      alice.lastOption match {
        case Some(lastAliceValue) =>
          val positionResult = positionWithIndex(lastAliceValue, result.headOption.getOrElse(1), 0, scores)
          println("index " + positionResult)
          climbingLeaderboard(scores.drop(positionResult._2), alice.dropRight(1), positionResult._1 +: result)
        case None => result
      }
    }

    @scala.annotation.tailrec
    private def positionWithIndex(aliceValue: Int, positionNr: Int, index: Int, scores: Array[Int]): (Int, Int) = {

      if (invalidValue(aliceValue)) throw new IllegalArgumentException("Wrong input")

      scores match {
        case Array(firstScore, _*) =>
          if (invalidValue(firstScore)) throw new IllegalArgumentException("Wrong input")
          if (aliceValue >= firstScore) {
            (positionNr, index)
          } else {
            scores.tail match {
              case Array(nextValue, _*) =>
                positionWithIndex(aliceValue, if (nextValue == firstScore) positionNr else positionNr + 1, index + 1, scores.tail)
              case _  => (positionNr + 1, index)
            }
          }
        case _ => (positionNr, index)
      }


    }

    private def invalidValue(value: Int) = {
      value < 0 || value > Math.pow(10, 9)
    }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val scoresCount = stdin.readLine.trim.toInt

    val scores = stdin.readLine.split(" ").map(_.trim.toInt)
    val aliceCount = stdin.readLine.trim.toInt

    val alice = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = climbingLeaderboard(scores, alice)

    printWriter.println(result.mkString("\n"))

    printWriter.close()
  }
}
