package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(n, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert and delete from empty heap should be an empty heap") = forAll { (a:Int) =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    h2 == empty
  }

  property("min of 2 element heap is min of elements") = forAll { (a1:Int, a2:Int) =>
    val h1 = insert(a1,empty)
    val h2 = insert(a2, h1)
    findMin(h2) == Math.min(a1, a2)
  }

  property("first two elements are retrieved in ascending order") = forAll { (h:H) =>
    if (!isEmpty(h)) {
      val a1 = findMin(h)
      val h2 = deleteMin(h)
      if (!isEmpty(h2)) {
        val a2 = findMin(h2)
        a1 <= a2
      }
      true
    }
    true
  }

  property("min of meld of two heaps is min of individual heaps' mins") = forAll {(h1:H, h2:H) =>
    val h3 = meld(h1, h2)
    findMin(h3) == Math.min(findMin(h1), findMin(h2))
  }

  property("min of meld of heap with empty heap is min of heap") = forAll { (h:H) =>
    val h2 = meld(h, empty)
    findMin(h2) == findMin(h)
  }

  property("elements are retrieved in ascending order") = forAll { (h: H) =>
    val elems = getAllElems(h)
    elems == elems.sorted
  }

  property("melded tree elements are retrieved in ascending order") = forAll { (h1:H, h2:H) =>
    val h3 = meld(h1, h2)
    val elems = getAllElems(h3)
    elems == elems.sorted
  }

  property("min of 1 element heap is the element") = forAll { (a:Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of meld of heap and empty heap is min of heap") = forAll {(h:H, l:List[Int]) =>
    val h2 = meld(h, empty)
    findMin(h2) == findMin(h)
  }

  property("meld of two heaps is same as meld of heaps with an element moved") = forAll { (h1:H,h2:H) =>
    def isEqual(h1:H, h2:H):Boolean ={
      (isEmpty(h1) && isEmpty(h2)) ||
      ((findMin(h1) == findMin(h2)) && (isEqual(deleteMin(h1), deleteMin(h2))))
    }
    isEqual(meld(h1, h2), meld(insert(findMin(h1),h2),deleteMin(h1)))
  }

  def getAllElems(h: H):List[Int] = {
    if (isEmpty(h)) Nil else findMin(h) :: getAllElems(deleteMin(h))
  }

}