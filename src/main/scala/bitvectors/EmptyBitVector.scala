package bitvectors

object EmptyBitVector extends BitVector {
  def &(bv: BitVector): BitVector = this
  def |(bv: BitVector): BitVector = bv
  def excl(position: Int): BitVector = this
  def ^(bv: BitVector): BitVector = bv
  def contains(position: Int): Boolean = false
  def cardinality: Int = 0
  def clearFrom(from: Int): BitVector = this
  def clearUntil(to: Int): BitVector = this
  override def filter(f: Int => Boolean): BitVector = this
  def filterBounds(f: Int => Boolean): BitVector = this
  def getWord(i: Int): Long = 0L
  def words: Array[Long] = Array()
  def intersects(bV: BitVector): Int = -1
  def intersects(bV: BitVector, position: Int): Boolean = false
  override def isEmpty: Boolean = true
  def lastSetBit: Int = -1
  def nbWords: Int = 0
  def nextSetBit(start: Int): Int = -1
  def prevSetBit(start: Int): Int = -1
  def setWordExpand(pos: Int, word: Long): BitVector = {
    if (pos == 0) {
      new SmallBitVector(word)
    } else {
      val array = new Array[Long](pos + 1)
      array(pos) = word
      LargeBitVector(array)
    }
  }
  def setWordShrink(pos: Int, word: Long): BitVector = ???
  def subsetOf(bv: BitVector): Boolean = true
  override def shift(n: Int): BitVector = this
}
