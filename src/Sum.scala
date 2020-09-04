object Sum {

  def simpleArraySum(ar: Array[Int]): Int = {
    val valid = ar.forall(_ <= 1000)
    if (!valid) throw new IllegalArgumentException("Item value should be no more than 1000")

    if (ar.length > 0) ar.sum else throw new IllegalArgumentException("Array shouldn't be empty")
  }

  def aVeryBigSum(ar: Array[Long]): Long = {
    val valid = ar.forall(item => item >= 0 && item <= Math.pow(10,10))
    if (!valid) throw new IllegalArgumentException("Item value invalid")

    if (ar.length > 0 && ar.length <= 10) ar.sum else throw new IllegalArgumentException("Array size invalid")
  }

}
