import scala.collection.mutable

object Result2 {

  /*
   * Complete the 'taxiDriver' function below.
   *
   * The function is expected to return a LONG_INTEGER.
   * The function accepts following parameters:
   *  1. LONG_INTEGER_ARRAY pickup
   *  2. LONG_INTEGER_ARRAY drop
   *  3. INTEGER_ARRAY tip
   */

  case class Range(min: Long, max: Long)

  case class Earn(ranges: List[Range], money: Long) {

    def overlap(newPickup: Long, newDrop: Long): Boolean = ranges.exists(range => (range.min <= newPickup && newPickup < range.max) || (range.min < newDrop && newDrop <= range.max))

  }



  implicit object EarnOrdering extends Ordering[Earn] {
    def compare(a:Earn, b:Earn): Int = a.money compare b.money
  }

  def taxiDriver(pickup: Array[Long], drop: Array[Long], tip: Array[Int]): Long = {
    var earns = mutable.Seq[Earn]()
    for(i <- pickup.indices) {
      earns = updateRanges(earns, pickup(i), drop(i), tip(i))
     // println(earns)
    }
    earns.max.money
  }

  def updateRanges(earns: mutable.Seq[Earn], pickup: Long, drop: Long, tip: Int): mutable.Seq[Earn] = {
    var result = earns
    val money = drop - pickup + tip

    for(i <- earns.indices) {
      if (!earns(i).overlap(pickup, drop)) {
        result = result :+ Earn(earns(i).ranges ++ List(Range(pickup, drop)), earns(i).money + money)
      }
    }

    result :+ Earn(List(Range(pickup, drop)), money)
  }

}

object Solution {
  def main(args: Array[String]) {
    System.out.println(Result2.taxiDriver(Array(11, 30, 0, 21, 41, 19), Array(20, 31, 17, 22, 46, 21), Array(6, 1, 9, 0, 8, 0))) //44
  }
}




