object NewYearChaos {

  // Complete the minimumBribes function below.
  def minimumBribes(q: Array[Int]): Unit = {
    println(minimumBribesInternal(q, 1, 2,3,"0"))
  }

  @scala.annotation.tailrec
  def minimumBribesInternal(q: Array[Int], expectedFirst: Int, expectedSecond: Int, expectedThird: Int, resultString: String): String = {
    q match {
      case Array(itemA, _*) =>
        if (itemA == expectedFirst) {
          minimumBribesInternal(q.tail, expectedSecond, expectedThird, expectedThird + 1, resultString)
        } else if (itemA == expectedSecond) {
          minimumBribesInternal(q.tail, expectedFirst, expectedThird, expectedThird + 1, (resultString.toInt + 1).toString)
        } else if (itemA == expectedThird) {
          minimumBribesInternal(q.tail, expectedFirst, expectedSecond, expectedThird + 1, (resultString.toInt + 2).toString)
        } else {
          "Too chaotic"
        }
      case _ => resultString
    }

  }


  def main(args: Array[String]): Unit = {
    minimumBribes(Array(2, 1, 5, 3, 4))
    minimumBribes(Array(2, 5, 1, 3, 4))
    minimumBribes(Array(1, 2, 5, 3, 4, 7, 8, 6)) //4
    minimumBribes(Array(1, 2, 5, 3, 7, 8, 6, 4)) //7
  }
}


