import scala.annotation.tailrec

object BubbleSort {
  def imperative(numbers: Array[Int]) = {
    for (k <- 1 until numbers.length; j <- 0 until numbers.length - k; if numbers(j) > numbers(j + 1)) {
      val x = numbers(j)
      numbers(j) = numbers(j + 1)
      numbers(j + 1) = x
    }
  }

  @tailrec
  def sort(numbers: List[Int], traversed: List[Int] = List.empty, swap: Boolean = false): List[Int] =
    numbers match {
      case Nil =>
        if (swap) sort(traversed, List.empty, swap = false)
        else traversed
      case x :: Nil => sort(Nil, traversed :+ x, swap)
      case x :: y :: rest =>
        if (x <= y) sort(y :: rest, traversed :+ x, swap)
        else sort(x :: rest, traversed :+ y, swap = true)
    }
}
