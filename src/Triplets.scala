object Triplets {

  def compareTriplets(a: Array[Int], b: Array[Int]): Array[Int] = {
    compare(Array(0,0), a, b)
  }

  def invalid(item: Int): Boolean = item < 1 || item > 100

  @scala.annotation.tailrec
  private def compare(result: Array[Int], a: Array[Int], b: Array[Int]): Array[Int] = {
    (a, b) match {
      case (Array(itemA, _*), Array(itemB, _*)) =>
        if (invalid(itemA) || invalid(itemB)) throw new IllegalArgumentException()
        if (itemA > itemB)
          compare(Array(result(0) + 1, result(1)), a.tail, b.tail)
        else if (itemB > itemA)
          compare(Array(result(0), result(1) + 1), a.tail, b.tail)
        else
          compare(result, a.tail, b.tail)
      case _ => result
    }
  }

}
