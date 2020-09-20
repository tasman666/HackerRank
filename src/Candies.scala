object Candies {

  // Complete the candies function below.
  def candies(n: Int, arr: Array[Int]): Long = {
    var candies: Array[Long] = Array.ofDim(arr.length)
    getMaxFromRight(arr, candies, 0)
    candies.sum
  }

  private def getMaxFromRight(input: Array[Int], candies: Array[Long], idx: Int): Long = {
    if (idx - 1 >= 0 && idx + 1 < input.length && input(idx - 1) < input(idx) && input(idx) > input(idx + 1)) {
      candies(idx) = Math.max(candies(idx-1), getMaxFromRight(input, candies, idx + 1)) + 1
    } else if (idx + 1 < input.length && input(idx + 1) < input(idx)) {
      candies(idx) = getMaxFromRight(input, candies, idx + 1) + 1
    } else if (idx - 1 >= 0 && input(idx - 1) < input(idx)) {
      candies(idx) = candies(idx - 1) + 1
      if (idx + 1 < input.length) {
        getMaxFromRight(input, candies, idx + 1)
      }
    } else {
      candies(idx) = 1
      if (idx + 1 < input.length) {
        getMaxFromRight(input, candies, idx + 1)
      }
    }
    candies(idx)
  }

  def main(args: Array[String]): Unit = {
    println(candies(4, Array(7, 8, 9, 8))) // 7
    println(candies(3, Array(1, 2, 1))) // 4
    println(candies(10, Array(2, 4, 2, 6, 1, 7, 8, 9, 2, 1))) // 19
    println(candies(8, Array(2, 4, 3, 5, 2, 6, 4, 5))) // 12
    println(candies(3, Array(9,6,3))) // 6
    println(candies(4, Array(9,6,3, 5))) // 8
    println(candies(4, Array(9,6,7, 5))) // 6
    println(candies(4, Array(1,2,3, 4))) // 10
  }
}

