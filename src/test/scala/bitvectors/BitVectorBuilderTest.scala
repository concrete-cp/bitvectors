package bitvectors

import org.scalatest.{FlatSpec, Matchers}

class BitVectorBuilderTest extends FlatSpec with Matchers {
  "BitVectorBuilder" should "set bits" in {
    val bvb = new BitVectorBuilder(1279)
    bvb.words.length shouldBe 20
  }
}
