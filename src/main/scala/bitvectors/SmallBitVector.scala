package bitvectors

import BitVector._

final class SmallBitVector private[bitvectors] (val word: Long) extends BitVector {
  def excl(position: Int): BitVector = setWord0(word & ~(1L << position))

  def contains(position: Int): Boolean = {
    position < WORD_SIZE && (word & (1L << position)) != 0L;
  }

  def prevSetBit(start: Int): Int = {
    val prev = if (start >= WORD_SIZE) {
      word
    } else {
      word & ~(MASK << start);
    }

    WORD_SIZE - java.lang.Long.numberOfLeadingZeros(prev) - 1
  }

  def lastSetBit: Int = {
    WORD_SIZE - java.lang.Long.numberOfLeadingZeros(word) - 1
  }

  def nextSetBit(start: Int): Int = {
    if (start >= WORD_SIZE) {
      -1
    } else {
      val next = word & (MASK << start);
      val ctz = java.lang.Long.numberOfTrailingZeros(next)
      if (ctz >= WORD_SIZE) {
        -1
      } else {
        ctz
      }
    }
  }

  def clearFrom(from: Int): BitVector = {
    if (from >= WORD_SIZE) {
      this
    } else if (from <= 0) {
      BitVector.empty
    } else {
      val newWord = word & ~(MASK << from)
      if (newWord == 0L) {
        EmptyBitVector
      } else {
        new SmallBitVector(newWord)
      }
    }
  }

  def clearUntil(until: Int): BitVector = {
    if (until >= WORD_SIZE) {
      BitVector.empty
    } else if (until < 0) {
      this
    } else {
      val newWord = word & (MASK << until)
      if (newWord == 0L) {
        EmptyBitVector
      } else {
        new SmallBitVector(newWord)
      }
    }
  }

  def intersects(bv: BitVector, position: Int): Boolean = {
    (bv.getWord(0) & word) != 0;
  }

  def intersects(bv: BitVector): Int = {
    if ((word & bv.getWord(0)) != 0) 0 else -1;
  }

  def nbWords: Int = 1

  def words: Array[Long] = Array(word)

  def ^(bv: BitVector): BitVector = {
    bv.setWordExpand(0, bv.getWord(0) ^ this.word)
  }

  def &(bv: BitVector): BitVector =
    setWord0(bv.getWord(0) & this.word)

  def |(bv: BitVector): BitVector = {
    val newWord = (bv.getWord(0) | this.word) //& (MASK >>> -size)
    bv.setWordExpand(0, newWord)
  }

  override def isEmpty: Boolean = {
    assert(word != 0L)
    false
  }

  def cardinality: Int = java.lang.Long.bitCount(word);

  def subsetOf(bv: BitVector): Boolean = {
    (word & ~bv.getWord(0)) == 0L;
  }

  def getWord(i: Int): Long = {
    if (i > 0)
      0L;
    else
      word;
  }

  private def setWord0(word: Long): BitVector = {
    if (word == this.word) {
      this
    } else if (word == 0L) {
      EmptyBitVector
    } else {
      new SmallBitVector(word)
    }
  }

  def setWordExpand(pos: Int, word: Long): BitVector = {
    if (pos == 0) {
      setWord0(word)
    } else {
      val array = new Array[Long](pos + 1)
      array(0) = this.word
      array(pos) = word
      LargeBitVector(array)
    }
  }

  def setWordShrink(pos: Int, word: Long): BitVector = {
    assert(pos == 0)
    setWord0(word)
  }

  override def filter(f: Int => Boolean): BitVector = {
    var newWord = 0L
    var i = nextSetBit(0)
    while (i >= 0) {
      if (f(i)) {
        newWord |= (1L << i)
      }
      i = nextSetBit(i + 1)
    }
    if (newWord == word) {
      this
    } else if (newWord == 0L) {
      EmptyBitVector
    } else {
      new SmallBitVector(newWord)
    }
  }

  def filterBounds(f: Int => Boolean): BitVector = {
    var newWord = word
    var i = nextSetBit(0)
    while (i >= 0 && !f(i)) {
      newWord &= ~(1L << i)
      i = nextSetBit(i + 1)
    }
    if (i >= 0) {
      i = lastSetBit
      while (i >= 0 && !f(i)) {
        newWord &= ~(1L << i)
        i = prevSetBit(i)
      }
    }

    if (newWord == word) {
      this
    } else {
      new SmallBitVector(newWord)
    }
  }
}
