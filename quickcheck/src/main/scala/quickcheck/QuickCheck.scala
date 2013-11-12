package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("two lists h, j such that min(h) > min(j), meld them, deleteMin is meld(h, deleteMin(j))") = forAll { (h: H, j: H) =>
    val m = meld(h, j)
    
    val res = deleteMin(m)
    equals(res, meld(h, deleteMin(j))) || equals(res, meld(j, deleteMin(h)))
  }
  
  val EMPTY = empty
  
  def equals(h:H, j:H): Boolean = (h, j) match {
    case (EMPTY, EMPTY) => true
    case (EMPTY, _) => false
    case (_, EMPTY) => false
    case (h, j) => findMin(h) == findMin(j) && equals(deleteMin(h), deleteMin(j))
  }
  
  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- Gen.frequency((1, empty), (10, genHeap))
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
