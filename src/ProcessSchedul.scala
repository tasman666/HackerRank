import java.util

import scala.util.Random

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
    val time = System.currentTimeMillis()
    util.Arrays.sort(ability)
    System.out.println(s"Sorting Time is ${System.currentTimeMillis() - time}")
    minimumTime(ability, Array(), processes, 0)
  }

  @scala.annotation.tailrec
  def minimumTime(ability: Array[Int], newValues: Array[Int], processes: Long, result: Int): Int = {
   // println(ability.mkString(", "))
    if (processes <= 0) {
      result
    } else {
      val maxValueAbility = if (ability.length > 0) ability(ability.length - 1) else 0
      val maxValueNewValues = if (newValues.length > 0) newValues(newValues.length - 1) else 0

      if (maxValueAbility >= maxValueNewValues) {
        val newValue = Math.floor(maxValueAbility / 2).toInt

        val newInput = newValues :+ newValue
        util.Arrays.sort(newInput)
        minimumTime(ability.dropRight(1), newInput, processes - maxValueAbility, result + 1)
      } else {
        val newValue = Math.floor(maxValueNewValues / 2).toInt
        val newInput = newValues.dropRight(1) :+ newValue
        util.Arrays.sort(newInput)
        minimumTime(ability, newInput, processes - maxValueNewValues, result + 1)
      }
//      val newValue = Math.floor(maxValue / 2).toLong
//      if (newValue >= ability(ability.length - 2)) {
//        minimumTime(ability.updated(ability.length - 1, newValue), newValues, processes - maxValue, result + 1)
//      } else {
//        minimumTime(ability.dropRight(1), (newValues :+ newValue).sorted, processes - maxValue, result + 1)
//      }
    }
  }

}

object Solution2 {
  def main(args: Array[String]) {
    System.out.println(Result.minimumTime(Array(2, 1, 5, 3, 1), 17))// 9
    System.out.println(Result.minimumTime(Array(3, 1, 7, 2, 4), 15)) // 4
    val array = createArray(20000000)
    val time = System.currentTimeMillis()
    System.out.println(Result.minimumTime(array, 20000000000L))
    System.out.println(s"Time is ${System.currentTimeMillis() - time}")
  }

  def createArray(size: Int): Array[Int] = {
    (0 until size).toArray.map(i => Random.nextInt(i + 100))
  }
}


