object MealPrice {

  // Complete the solve function below.
  def solve(meal_cost: Double, tip_percent: Int, tax_percent: Int): Long = {
    val totalCost = meal_cost + (meal_cost * tip_percent / 100) + (meal_cost * tax_percent / 100)
    Math.round(totalCost)
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn 

    val meal_cost = stdin.readLine.trim.toDouble

    val tip_percent = stdin.readLine.trim.toInt

    val tax_percent = stdin.readLine.trim.toInt

    println(solve(meal_cost, tip_percent, tax_percent))
  }
}

