object MakingCandies {

  // Complete the minimumPasses function below.
  def minimumPasses(m: Long, w: Long, p: Long, n: Long): Long = {
    if (invalid(m, w, p, n)) throw new IllegalArgumentException("Invalid input")
    minimumPasses(m,w,p,n, 0,0)
  }

  private def invalid(values: Long*): Boolean = {
    values.exists(value =>
      value < 1 || value > Math.pow(10, 12))
  }

  @scala.annotation.tailrec
  private def minimumPasses(m: Long, w: Long, p: Long, n: Long, candies: Long, passes: Int): Long = {
    candies match {
      case c if c >= n || c < 0 => passes
      case c if c + (m * w) >= n || c < p => // no workers or machines
        minimumPasses(m, w, p, n, c + (m * w), passes + 1)
      case c =>  // buying  workers or machines
        val newResources = c / p

        val resourceResult = calculateResource(newResources, m, w)
        val currentMachines = resourceResult._1
        val currentWorkers = resourceResult._2

        val restCandies = candies - (p * newResources)
        minimumPasses(currentMachines, currentWorkers, p, n, restCandies + (currentMachines * currentWorkers), passes + 1)
    }

  }

  @scala.annotation.tailrec
  private def calculateResource(newResources: Long, machines: Long, workers: Long): (Long, Long) = {
    machines match {
      case m if m == workers =>
        val half = newResources / 2
        (machines + half, workers + (newResources - half))
      case m if m > workers =>
        if (newResources <= machines - workers) {
          (machines, workers + newResources)
        } else {
          calculateResource(newResources - (machines - workers), machines, workers + (machines - workers))
        }
      case m if m < workers =>
        if (newResources <= workers - machines) {
          (machines + newResources, workers)
        } else {
          calculateResource(newResources - (workers - machines), machines + (workers - machines), workers)
        }
    }

  }

  def main(args: Array[String]): Unit = {
    println(123456789012L * 215987654321L)
    println(minimumPasses(3, 1, 2, 12)) // 3
    println(minimumPasses(1, 1, 6, 45)) // 16
    println(minimumPasses(1, 100, 10000000000L, 1000000000000L)) // 617737754
    println(minimumPasses(123456789012L, 215987654321L, 50000000000L, 1000000000000L)) // 1
    println(minimumPasses(5,1, 172, 364)) // 72
  }
}

