

object SherlockAndAnagrams {


  // Complete the sherlockAndAnagrams function below.
  def sherlockAndAnagrams(s: String): Int = {

    var keys = Seq[String]()
    val allTokens = s.foldLeft(Seq[String]()) ( (tokens, char) => {
      val newTokens = Seq("" + char) ++ keys.map(_ + char)

      keys = newTokens
      newTokens ++ tokens

    })
    findAnagrams(allTokens,Seq()).size
  }

  @scala.annotation.tailrec
  private def findAnagrams(source: Seq[String], anagrams: Seq[String]): Seq[String] = {
    source match {
      case Nil => anagrams
      case head :: tail =>  findAnagrams(tail, anagrams ++ checkAnagrams(head, tail))
    }
  }

  private def checkAnagrams(source: String, tokens: Seq[String]): Seq[String] = {
    tokens.filter(token => {
      if (token.length == source.length) {

        isAnagram(source, token)

      } else {
        false
      }
    })
  }

  private def isAnagram(source: String, target: String): Boolean = {
    var tokenVar = target
    for (i <- source.indices) {
      val index = tokenVar.indexOf(source.charAt(i))
      if (index == -1) {
        return false
      } else {
        if (tokenVar.length != 1) {
          tokenVar = tokenVar.substring(0, index) + tokenVar.substring(index + 1)
        }
      }
    }
    true
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