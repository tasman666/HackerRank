object IceCreamParlor {

  // Complete the whatFlavors function below.
  def whatFlavors(cost: Array[Int], money: Int) {
    val result = whatFlavors(cost, 0, money, None)
    println(result._1 + " " + result._2)
  }

  @scala.annotation.tailrec
  private def whatFlavors(cost: Array[Int], dropNr: Int, money: Int, result: Option[(Int, Int)]): (Int, Int) = {
    result match {
      case None =>
        val resultArray = for (
          (value1, index1) <- cost.drop(dropNr).zipWithIndex;
          (value2, index2) <- cost.drop(dropNr + 1).zipWithIndex
          if value1 + value2 == money
        ) yield {
          (index1 + 1 + dropNr, index2 + 1 + dropNr + 1)
        }
        whatFlavors(cost, dropNr + 1, money, resultArray.headOption)
      case Some((value1, value2)) => (value1, value2)
    }
  }

  def main(args: Array[String]): Unit = {
    whatFlavors(Array(1,4,6,5,3),11)
  }

}
