import scala.annotation.tailrec

sealed abstract class Heap[+A] { def rank: Int } 

case object EmptyHeap extends Heap[Nothing] { def rank = 0 }
case class NonEmptyHeap[A](rank: Int, element: A, left: Heap[A], right: Heap[A]) extends Heap[A]

/**
*
* Time complexity O(n log n)
*/
object HeapSort {
  def apply[A](x: A): Heap[A] =
    this(x, EmptyHeap, EmptyHeap)

  def apply[A](x: A, a: Heap[A], b: Heap[A]): Heap[A] =
    if (a.rank > b.rank) NonEmptyHeap(b.rank + 1, x, a, b)
    else NonEmptyHeap(a.rank + 1, x, b, a)

  def merge(a: Heap[Int], b: Heap[Int]): Heap[Int] =
    (a, b) match {
      case (x, EmptyHeap) => x
      case (EmptyHeap, x) => x
      case (x: NonEmptyHeap[Int], y: NonEmptyHeap[Int]) =>
        if (x.element >= y.element) HeapSort(x.element, x.left, merge(x.right, y))
        else HeapSort(y.element, y.left, merge(x, y.right))
    }

  def toList(heap: Heap[Int]) =
    toListWithMemory(List(), heap)

  @tailrec
  def toListWithMemory(memo: List[Int], heap: Heap[Int]): List[Int] =
    heap match {
      case EmptyHeap => memo
      case x: NonEmptyHeap[Int] =>
        toListWithMemory(x.element :: memo, merge(x.left, x.right))
    }

  def sort(xs: Seq[Int]): Seq[Int] =
    toList(xs.foldLeft(EmptyHeap: Heap[Int])((memo, x) => merge(HeapSort(x), memo)))
}

object HeapSortImperative {
  def heapify(a: Array[Int], n: Int, i: Int): Unit = {
    var largest = i
    var l = 2 * i + 1
    var r = 2 * i + 2
    
    if (l < n && a(i) < a(l)) {
      largest = l
    }
    
    if (r < n && a(largest) < a(r)) {
      largest = r
    }
    
    if (largest != i) {
      val tmp = a(i)
      a(i) = a(largest)
      a(largest) = tmp
      
      heapify(a, n, largest)
    }
  }
  
  def sort(a: Array[Int]) = {
    val n = a.length

    for (i <- n / 2 - 1 to 0 by -1 ) {
      println(i)
      heapify(a, n, i)
    }
    
    for (i <- n - 1 to 0 by -1) {
      val tmp = a(i)
      a(i) = a(0)
      a(0) = tmp
      
      heapify(a, i, 0)
    }
  } 
}
