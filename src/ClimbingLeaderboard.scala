import java.io.PrintWriter
import java.util

object ClimbingLeaderboard {

  // Complete the climbingLeaderboard function below.
    def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {
      if (invalidArray(scores) || invalidArray(alice)) {
        throw new IllegalArgumentException("Wrong input")
      }

      val scoresDist = scores.reverse.distinct
      alice.map(aliceValue => {
        val binarySearchResult = util.Arrays.binarySearch(scoresDist, aliceValue)
        if (binarySearchResult < 0) {
          (scoresDist.length + 1) -  (Math.abs(binarySearchResult) - 1)
        } else {
          scoresDist.length - binarySearchResult
        }
      })

    }

    private def invalidArray(array: Array[Int]) = {
      array.length < 1 || array.length > 2 * Math.pow(10, 5)
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
