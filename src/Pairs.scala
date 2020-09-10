import IceCreamParlor.whatFlavors

object Pairs {

  // Complete the pairs function below.
  def pairs(k: Int, arr: Array[Int]): Int = {
    val set = scala.collection.mutable.HashSet[Int]()
    var result = 0
    for(i <- arr.indices) {
      if (set.contains(arr(i) - k)) {
        result += 1
      }
      if (set.contains(arr(i) + k)) {
        result += 1
      }
      set += arr(i)
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(pairs(2, Array(1,5,3,4,2)))
  }
}

//3, -1, 7, 3