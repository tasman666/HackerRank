object HackerRank {


  def main(args: Array[String]): Unit = {

    println(Triplets.compareTriplets(Array(87, 54, 23), Array(67, 68, 21)).mkString(", "))
    println(Sum.simpleArraySum(Array(87, 54, 23)))
    println(Sum.aVeryBigSum(Array(87, 54, 23)))

    println(ClimbingLeaderboard.climbingLeaderboard(Array(100,50,50,30,20, 10), Array(5, 10,50,120)).mkString(", "))
  }

}
