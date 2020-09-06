import java.io.PrintWriter

object ClimbingLeaderboard {

  // Complete the climbingLeaderboard function below.
  def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {

    alice.map(aliceValue => {
      position(aliceValue, 1, scores)
    })

  }

  @scala.annotation.tailrec
  private def position(aliceValue: Int, positionNr: Int, scores: Array[Int]): Int = {

    scores match {
      case Array(firstScore, _*) =>
        if (aliceValue >= firstScore) {
          positionNr
        } else {
          scores.tail match {
            case Array(nextValue, _*) =>
              if (nextValue == firstScore) {
                position(aliceValue, positionNr, scores.tail)
              } else {
                position(aliceValue, positionNr + 1, scores.tail)
              }
            case _  => positionNr + 1
          }
        }
      case _ => positionNr
    }


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
