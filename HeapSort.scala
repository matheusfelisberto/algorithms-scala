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
