object MakingCandies {

  // Complete the minimumPasses function below.
  def minimumPasses(m: Long, w: Long, p: Long, n: Long): Long = {
    minimumPasses(m,w,p,n, 0,0)
  }

  @scala.annotation.tailrec
  private def minimumPasses(m: Long, w: Long, p: Long, n: Long, candies: Long, passes: Int): Long = {
    candies match {
      case c if c >= n => passes
      case c if c + (m * w) >= n || c < p => // no workers or machines
        minimumPasses(m, w, p, n, c + (m * w), passes + 1)
      case c =>  // buying  workers or machines
        val newResources = c / p
        val currentMachines = if (m < w) newResources + m else m
        val currentWorkers = if (w <= m) newResources + w else w
        val restCandies = candies - (p * newResources)
        minimumPasses(currentMachines, currentWorkers, p, n, restCandies + (currentMachines * currentWorkers), passes + 1)
    }

  }

  def main(args: Array[String]): Unit = {
    println(minimumPasses(3, 1, 2, 12)) // 3
    println(minimumPasses(1, 1, 6, 45)) // 16
  }
}
