object NewYearChaos {

  // Complete the minimumBribes function below.
  def minimumBribes(q: Array[Int]): Unit = {
    println(minimumBribesInternal(q, "0"))
  }

  @scala.annotation.tailrec
  def minimumBribesInternal(q: Array[Int], resultString: String): String = {
    q match {
      case Array(itemA, _*) =>
        val result = q.tail.count(_ < itemA)
        if (result > 2) {
          "Too chaotic"
        } else {
          minimumBribesInternal(q.tail, (resultString.toInt + result).toString)
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


