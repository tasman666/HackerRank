import IceCreamParlor.whatFlavors

object LeftRotation {

  // Complete the rotLeft function below.
  def rotLeft(a: Array[Int], d: Int): Array[Int] = {

    a.zipWithIndex.map { case (_, index) =>
      a((index + d) % a.length)
    }

  }


  def main(args: Array[String]): Unit = {
    println(rotLeft(Array(1,4,6,5,3),1).mkString(","))
    println(rotLeft(Array(1,4,6,5,3),2).mkString(","))
    println(rotLeft(Array(1,4,6,5,3),4).mkString(","))
  }
}

// 14653 -> 46531
//0 -> 4
//1 -> 0
//2 -> 1
//3 -> 2
//4 -> 3