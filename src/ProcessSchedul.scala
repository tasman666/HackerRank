import java.util

object Result {

  /*
   * Complete the 'minimumTime' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts following parameters:
   *  1. INTEGER_ARRAY ability
   *  2. LONG_INTEGER processes
   */

  def minimumTime(ability: Array[Int], processes: Long): Int = {
    minimumTime(ability.map(_.toLong), processes, 0)
  }

  @scala.annotation.tailrec
  def minimumTime(ability: Array[Long], processes: Long, result: Int): Int = {
    if (processes <= 0) {
      result
    } else {
      val sorted = ability.sorted
      val maxValue = sorted(sorted.length - 1)
      minimumTime(sorted.updated(sorted.length - 1, Math.floor(maxValue / 2).toLong), processes - maxValue, result + 1)
    }
  }

}

object Solution2 {
  def main(args: Array[String]) {
    System.out.println(Result.minimumTime(Array(2, 1, 5, 3, 1), 17))// 9
    System.out.println(Result.minimumTime(Array(3, 1, 7, 2, 4), 15)) // 4
  }
}