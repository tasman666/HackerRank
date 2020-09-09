
object IceCreamParlor {

  // Complete the whatFlavors function below.
  def whatFlavors(cost: Array[Int], money: Int) {
    val mapA = scala.collection.mutable.HashMap[Int, Int]()
    for(i <- cost.indices) {
        if (mapA.contains(money - cost(i))) {
          println((mapA(money - cost(i)) + 1) + " " + (i + 1))
          return
        } else {
          mapA += (cost(i) -> i)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    whatFlavors(Array(1,4,6,5,3),11)
    whatFlavors(Array(1,4,6,5,3),5)
    whatFlavors(Array(1,4,6,5,3),9)
  }

}
