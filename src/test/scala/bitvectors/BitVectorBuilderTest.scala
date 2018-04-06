package bitvectors

import java.util

import org.scalatest.{FlatSpec, Matchers}

class BitVectorBuilderTest extends FlatSpec with Matchers {
  "BitVectorBuilder" should "set bits" in {
    val bvb = new util.BitSet(1279)
    bvb.toLongArray.length shouldBe 20
  }
}
