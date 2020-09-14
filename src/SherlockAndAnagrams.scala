

object SherlockAndAnagrams {

  implicit class RichInt(val n : Int) {
    def ! : Int = (1 to n).product
  }


  // Complete the sherlockAndAnagrams function below.
  def sherlockAndAnagrams(s: String): Int = {
    var result = 0

    val map = s.foldLeft( (Set[String](),Map[String, Int]()) ) ( (tuple, char) => {
      val keys = tuple._1
      val keysWithCounts = tuple._2

      val newKeysWithCounts = keys.map(f = key => {
        key + "" + char -> (keysWithCounts.getOrElse(key + "" + char, 0) + 1)
      })

      val resultMap: Map[String, Int] = {
        keysWithCounts ++ newKeysWithCounts + ("" + char -> (keysWithCounts.getOrElse("" + char, 0) + 1))
      }

      val newKeys = newKeysWithCounts.map(_._1) + ("" + char)
      println(newKeys)
      (newKeys, resultMap)
    })

    val counts = map._2

    println(counts)
    val set = scala.collection.mutable.HashSet[String]()
    counts.foreach( a => {
      val token = a._1
      val value = a._2
      if (token == token.reverse) {
        val valueToAdd = if(value > 1) (value - 1)! else 0
        result += valueToAdd
      } else {
        if(!set.contains(token)) {
          val reverseValue = counts.getOrElse(token.reverse, 0)
          val valueToAdd = if(value + reverseValue > 1) (value + reverseValue - 1)! else 0
          result += valueToAdd
          set += token
          set += token.reverse
        }
      }
    })



    result
  }


  def main(args: Array[String]): Unit = {
    println(sherlockAndAnagrams("ifailuhkqq")) // 3
    println(sherlockAndAnagrams("kkkk")) // 10
    println(sherlockAndAnagrams("abba")) // 4
    println(sherlockAndAnagrams("abcd")) // 0
  }
}


// a | ab, b | abb, bb, b | abba, bba, ba, a
// i | if, f | ifa,fa, i  |