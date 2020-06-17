object BubbleSort {
  def sort(numbers: Array[Int]) = {
    for (k <- 1 until numbers.length; j <- 0 until numbers.length - k; if numbers(j) > numbers(j + 1)) {
      val x = numbers(j)
      numbers(j) = numbers(j + 1)
      numbers(j + 1) = x
    }
  }
}
