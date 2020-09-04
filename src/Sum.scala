object Sum {

  def simpleArraySum(ar: Array[Int]): Int = {
    val valid = ar.forall(_ <= 1000)
    if (!valid) throw new IllegalArgumentException("Item value should be no more than 1000")

    if (ar.length > 0) ar.sum else throw new IllegalArgumentException("Array shouldn't be empty")
  }

}
