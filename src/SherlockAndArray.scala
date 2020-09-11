
object SherlockAndArray {

  def balancedSums(arr: Array[Int]): String = {
    if (arr.length == 1) {
      "YES"
    } else {
      val sum = arr.sum
      var left = 0
      for (i <- arr.indices) {
        if (sum - arr(i) - left == left) {
          return "YES"
        }
        left += arr(i)
      }
      "NO"
    }
  }

  def main(args: Array[String]): Unit = {
    println(balancedSums(Array(1, 1, 4, 1, 1)))
    println(balancedSums(Array(2, 0, 0, 0)))
    println(balancedSums(Array(0, 0, 2, 0)))
    println(balancedSums(Array(1, 2 ,3)))
  }

}
