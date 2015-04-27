package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }

  property("sorted") = forAll { l: List[A] =>
    val h = l.foldRight(empty)(insert)
    def asList(h: H): List[A] = {
      if (isEmpty(h)) List[A]() else findMin(h)::asList(deleteMin(h))
    }
    asList((h)) == l.sorted
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
