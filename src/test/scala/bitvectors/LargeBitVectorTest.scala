package bitvectors

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

final class LargeBitVectorTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  "LargeBitVectors" should "be filled" in {
    val bitVector = BitVector.filled(125)
    assert(bitVector(64))
    assert(bitVector(65))
    assert(bitVector(124))
    assert(!bitVector(125))
    bitVector.nextSetBit(0) shouldBe 0

    BitVector.empty.nextSetBit(0) shouldBe -1

    BitVector.filled(0) shouldBe BitVector.empty

    val bv2 = BitVector.empty + 0
    bv2.nextSetBit(0) shouldBe 0
    bv2.nextSetBit(1) shouldBe -1

    val bv3 = BitVector.empty + 64
    bv3.nextSetBit(0) shouldBe 64
  }

  it should "compute correct number of words" in {
    BitVector.word(0) shouldBe 0
    BitVector.word(1) shouldBe 0
    BitVector.word(64) shouldBe 1
    BitVector.word(65) shouldBe 1
  }

  it should "set bits" in {
    var bitVector = BitVector.empty
    assert(!bitVector(100))
    bitVector += 100
    assert(bitVector(100))
    bitVector = bitVector - 100
    assert(!bitVector(100))
  }

  it should "shrink" in {
    var bitVector = BitVector.empty + 16 + 100
    bitVector -= 100
    bitVector shouldBe a[SmallBitVector]
    bitVector.iterator.toSeq should contain theSameElementsAs Seq(16)
    bitVector - 16 shouldBe BitVector.empty

    BitVector(Array(10L, 10L)) shouldBe a[LargeBitVector]
    BitVector(Array(987L, 0L)) shouldBe a[SmallBitVector]
    BitVector(Array(0L, 0L)) shouldBe EmptyBitVector

  }

  it should "get bits" in {
    val bitVector = BitVector.empty + 46
    assert(!bitVector(0))
    assert(!bitVector(45))
    assert(bitVector(46))
  }

  it should "compute next bit" in {
    val bitVector = BitVector.empty + 46 + 49 + 100
    bitVector.nextSetBit(0) shouldBe 46
    bitVector.nextSetBit(46) shouldBe 46
    bitVector.nextSetBit(47) shouldBe 49
    bitVector.nextSetBit(63) shouldBe 100
    bitVector.nextSetBit(64) shouldBe 100
    bitVector.nextSetBit(101) shouldBe -1
  }

  it should "compute next bit 65" in {
    val bitVector = BitVector.empty + 65
    bitVector.nextSetBit(2) shouldBe 65
  }
  //
  //  it should "compute prev cleared bit" in {
  //    var bitVector = BitVector.filled(125) - 46 - 49 - 100
  //
  //    bitVector.prevClearBit(47) shouldBe 46
  //    bitVector.prevClearBit(46) shouldBe -1
  //    bitVector.prevClearBit(45) shouldBe -1
  //    bitVector.prevClearBit(110) shouldBe 100
  //
  //    bitVector.prevClearBit(64) shouldBe 49
  //    bitVector.prevClearBit(63) shouldBe 49
  //
  //    bitVector -= 64
  //    bitVector.prevClearBit(65) shouldBe 64
  //    bitVector.prevClearBit(64) shouldBe 49
  //
  //    bitVector += 64
  //    bitVector -= 63
  //    bitVector.prevClearBit(65) shouldBe 63
  //    bitVector.prevClearBit(63) shouldBe 49
  //
  //  }

  it should "compute prev set bit" in {
    var bitVector = BitVector.empty + 46 + 49 + 100;

    bitVector.lastSetBit shouldBe 100

    bitVector.prevSetBit(47) shouldBe 46
    bitVector.prevSetBit(46) shouldBe -1
    bitVector.prevSetBit(45) shouldBe -1
    bitVector.prevSetBit(110) shouldBe 100

    bitVector.prevSetBit(64) shouldBe 49
    bitVector.prevSetBit(63) shouldBe 49

    bitVector += 64
    bitVector.prevSetBit(65) shouldBe 64
    bitVector.prevSetBit(64) shouldBe 49

    bitVector -= 64
    bitVector += 63
    bitVector.prevSetBit(65) shouldBe 63
    bitVector.prevSetBit(63) shouldBe 49

  }

  it should "be converted to String" in {
    val bitVector = BitVector.empty + 46 + 49 + 100
    bitVector.toString shouldBe "LargeBitVector{46, 49, 100}"
  }

  it should "compute correct word positions" in {
    BitVector.word(0) shouldBe 0
    BitVector.word(63) shouldBe 0
    BitVector.word(64) shouldBe 1
  }

  it should "correctly clear parts from bit" in {

    BitVector(Set(27860, 65929, 41689, 94054, 57759, 35436, 56080, 80650, 70344, 24299, 28787)).clearFrom(41728) should
      contain theSameElementsAs Set(27860, 41689, 35436, 24299, 28787)

    val bitVector = BitVector.empty + 46 + 49 + 100
    bitVector.clearFrom(101) shouldBe bitVector
    bitVector.clearFrom(128) shouldBe bitVector
    bitVector.clearFrom(1000) shouldBe bitVector

    assert((BitVector.empty + 128).clearFrom(128).isEmpty)
    assert((BitVector.empty + 129).clearFrom(128).isEmpty)
    assert((BitVector.empty + 127).clearFrom(128)(127))

    val bv2 = bitVector.clearFrom(47)
    bv2 should not be bitVector
    bv2.cardinality shouldBe 1
    assert(bv2(46))
    assert(!bv2(49))
    assert(!bv2(100))
  }

  it should "correctly clear parts until bit" in {
    val bitVector = BitVector.empty + 46 + 49 + 100
    bitVector.clearUntil(45) shouldBe bitVector

    val bv2 = bitVector.clearUntil(49)
    bv2 should not be bitVector
    bv2.cardinality shouldBe 2
    assert(!bv2(46))
    assert(bv2(49))
    assert(bv2(100))
  }

  it should "correctly set parts from bit" in {
    val bitVector = BitVector.empty.set(80, 125)

    bitVector.cardinality shouldBe 45

    for (i <- 0 until 80) {
      assert(!bitVector(i));
    }
    for (i <- 80 until 125) {
      assert(bitVector(i));
    }
    for (i <- 125 until 200) {
      assert(!bitVector(i));
    }

    val bv = BitVector.filled(2000)
    bv.set(100, 2000) shouldBe bv
  }

  it should "detect subsets" in {
    val bitVector = BitVector.empty + 46 + 49 + 100

    val bv2 = BitVector.empty + 46

    assert(!bitVector.subsetOf(bv2));
    assert(bv2.subsetOf(bitVector));

    var bv3 = BitVector.empty
    var bv4 = BitVector.empty
    for (i <- 0 until 70) {
      bv3 += i;
      bv4 += i;
    }
    assert(bv3.subsetOf(bv4));
    assert(bv4.subsetOf(bv3));
  }

  it should "have correct hash codes" in {
    val bitVector = BitVector.empty + 46

    val bv2 = BitVector.empty + 46

    val bv3 = BitVector.empty + 46

    bitVector shouldBe bv2
    bitVector shouldBe bv3
    bitVector.hashCode shouldBe bv2.hashCode
    bitVector.hashCode shouldBe bv3.hashCode

  }

  it should "compute xor" in {
    val bitVector = BitVector.empty + 59 + 11

    val bv2 = BitVector.empty + 10 + 11

    val bv3 = bitVector ^ bv2
    val bv4 = bv2 ^ bitVector

    bv3 shouldBe bv4
    bv2.cardinality shouldBe 2

    assert(bv3(10))
    assert(!bv3(11))
    assert(bv3(59))

  }
  it should "filter" in {
    val bv = BitVector.empty + 127
    val filt: BitVector = bv.filter(_ % 2 == 1)

    val bitVector = BitVector.filled(128)
    val filtered = bitVector.filter(_ % 2 == 1)
    filtered.cardinality shouldBe 64
    filtered.lastSetBit shouldBe 127
    filtered.nextSetBit(0) shouldBe 1
  }

  it should "retrieve entries" in {
    var bv = BitVector.empty
    bv += 203
    bv += 202
    bv += 134
    bv += 104
    bv += 86
    bv += 50
    bv += 38
    bv += 26

    for (e <- Seq(203, 202, 134, 104, 86, 50, 38, 26)) {
      assert(bv(e))
    }

    bv.iterator.toSeq should contain theSameElementsAs Seq(203, 202, 134, 104, 86, 50, 38, 26)
    bv.lastSetBit shouldBe 203
  }

  it should "filter bounds" in {
    val bv = BitVector(Seq(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))

    bv.filterBounds(_ > 10).iterator.toSeq shouldBe Seq(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
    bv.filterBounds(_ < 30).iterator.toSeq shouldBe Seq(5, 10, 15, 20, 25)
  }

  it should "behave as sets" in {
    val gen: Gen[Set[Int]] = Gen.buildableOf[Set[Int], Int](Gen.choose(0, 100000))

    forAll(gen, Gen.choose(0, 100000)) { (ps, c) =>
      BitVector(ps) should contain theSameElementsAs ps
      BitVector(ps).clearFrom(c) should contain theSameElementsAs ps.filter(_ < c)
    }
  }

  it should "behave as sets, basic case" in {
    BitVector(Seq(0)) should contain theSameElementsAs Seq(0)
  }

  it should "shift" in {
    val ps = BitVector(Set(82, 24, 51, 89, 52))
    val c = 64

    ps.shift(c).iterator.toSeq should contain theSameElementsAs
      ps.iterator.toSeq.map(_ + c)

    BitVector(Set(82091, 24461, 51961, 89508, 52141)).shift(192).iterator.toSeq should contain theSameElementsAs
      Set(82283, 52153, 89700, 52333, 24653)

    val gen: Gen[Set[Int]] = Gen.buildableOf[Set[Int], Int](Gen.choose(0, 100000))
    forAll(gen, Gen.choose(-1000, 1000)) { (ps, c) =>
      val bv = BitVector(ps)
      val sh = bv.shift(c)
      sh.iterator.toSeq should contain theSameElementsAs ps.map(_ + c).filter(_ >= 0)
    }
  }

  it should "also shift" in {
    val bv = BitVector(Seq(146, 160, 174))
    bv.shift(-146).iterator.toSeq should contain theSameElementsAs Seq(0, 14, 28)

  }

  it should "shift and clear" in {
    BitVector(Seq(1, 10, 30, 60, 100)).shift(-20) should contain theSameElementsAs Seq(10, 40, 80)
  }
}
