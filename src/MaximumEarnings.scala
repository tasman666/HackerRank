import scala.collection.mutable
import scala.util.Random

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

  case class Earn(pickup: Long, drop: Long, money: Long) {
    def overlap(earn: Earn): Boolean = earn.pickup < drop
  }

  implicit object EarnOrdering extends Ordering[Earn] {
    def compare(a:Earn, b:Earn): Int = a.pickup compare b.pickup
  }

  object EarnOrderingMoney extends Ordering[Earn] {
    def compare(a:Earn, b:Earn): Int = a.money compare b.money
  }

  def taxiDriver(pickup: Array[Long], drop: Array[Long], tip: Array[Int]): Long = {
    val earns = mutable.TreeSet[Earn]()
    for(i <- pickup.indices) {
      val money = drop(i) - pickup(i) + tip(i)
      earns.add( Earn(pickup(i), drop(i), money) )
    }

    var earnChains = mutable.Seq[Earn]()
    earns.foreach(earn => earnChains = updateEarnChains(earnChains, earn))
    earnChains.max(EarnOrderingMoney).money
  }

  def updateEarnChains(earns: mutable.Seq[Earn], earn: Earn): mutable.Seq[Earn] = {
    var result = earns
    for(i <- earns.indices) {
      if (!earns(i).overlap(earn)) {
        result = result :+ Earn(earns(i).pickup, earn.drop, earns(i).money + earn.money)
      }
    }

    result = result :+ earn
    val rest = if (result.exists(e => e.drop <= earn.pickup)) {
      Seq(result.filter(e => e.drop <= earn.pickup).max(EarnOrderingMoney))
    } else  Seq()
    result.filter(e => e.drop > earn.pickup) ++ rest
  }

}

object Solution {
  def main(args: Array[String]) {
    System.out.println(Result2.taxiDriver(Array(11, 30, 0, 21, 41, 19), Array(20, 31, 17, 22, 46, 21), Array(6, 1, 9, 0, 8, 0))) //44


    val (pick,drop) = createArrays(10000)
    val tip = createArray(10000)
    val time = System.currentTimeMillis()
    System.out.println(Result2.taxiDriver(pick, drop, tip))
    System.out.println(s"Time is ${System.currentTimeMillis() - time}")
  }

  def createArrays(size: Int): (Array[Long],Array[Long]) = {
    val pick = Array.ofDim[Long](size)
    val drop = Array.ofDim[Long](size)
    (0 until size).toArray.foreach {
      i => {
        val randomValue = Random.nextLong()
        pick.update(i, randomValue)
        drop.update(i, randomValue + Random.nextInt(10))
      }
    }
    (pick, drop)
  }

  def createArray(size: Int): Array[Int] = {
    (0 until size).toArray.map(i => Random.nextInt(i + 100))
  }
}




