package bitvectors

import java.util

import bitvectors.BitVector._

import scala.collection.SortedSetLike
import scala.collection.immutable.SortedSet

object BitVector {

  val MASK: Long = 0xFFFFFFFFFFFFFFFFL
  private val ADDRESS_BITS_PER_WORD: Int = 6
  val WORD_SIZE: Int = 1 << ADDRESS_BITS_PER_WORD


  def filled(size: Int): BitVector = empty.set(0, size)

  def empty: BitVector = EmptyBitVector

  def word(bit: Int): Int = bit >> ADDRESS_BITS_PER_WORD

  def apply(v: Traversable[Int]): BitVector = {
    val bvb = new util.BitSet()
    for (b <- v) {
      bvb.set(b)
    }
    BitVector(bvb)
  }


  def apply(bitSet: util.BitSet): BitVector = {
    apply(bitSet.toLongArray)
  }

  def apply(words: Array[Long]): BitVector = apply(words, words.length)

  def apply(words: Array[Long], until: Int): BitVector = {
    var trimTo = until

    while (trimTo > 0 && words(trimTo - 1) == 0L) {
      trimTo -= 1
    }

    trimTo match {
      case 0 => EmptyBitVector
      case 1 => new SmallBitVector(words(0))
      case _ =>
        val nw = if (trimTo == words.length) words else util.Arrays.copyOf(words, trimTo)
        LargeBitVector(nw)
    }

  }
}

trait BitVector extends SortedSet[Int] with SortedSetLike[Int, BitVector] {

  //  def traversable: Traversable[Int] = new Traversable[Int] {
  //    def foreach[U](f: Int => U): Unit = BitVector.this.foreach(f)
  //  }

  override def empty: BitVector = BitVector.empty

  override def foreach[U](f: Int => U): Unit = {
    var i = nextSetBit(0)
    while (i >= 0) {
      f(i)
      i = nextSetBit(i + 1)
    }
  }

  def set(from: Int, until: Int): BitVector = {
    val bvb = util.BitSet.valueOf(words)
    bvb.set(from, until)

    if (bvb.cardinality > cardinality) {
      BitVector(bvb)
    } else {
      this
    }
  }


  def words: Array[Long]

  def -(position: Int): BitVector

  def +(position: Int): BitVector = {
    val wordPos = word(position)
    val oldWord = getWord(wordPos)
    val newWord = oldWord | (1L << position)

    if (oldWord == newWord) {
      this
    } else {
      setWordExpand(wordPos, newWord)
    }
  }

  def ++(p: Traversable[Int]): BitVector = {
    val bvb = util.BitSet.valueOf(words)

    for (i <- p) {
      bvb.set(i)
    }

    if (bvb.cardinality != cardinality) {
      BitVector(bvb)
    } else {
      this
    }
  }

  def --(p: Traversable[Int]): BitVector = p.foldLeft(this)(_ - _)

  // def apply(position: Int): Boolean

  def nextSetBit(start: Int): Int

  def prevSetBit(start: Int): Int

  def lastSetBit: Int

  def clearFrom(from: Int): BitVector

  def clearUntil(until: Int): BitVector

  def intersects(bV: BitVector, position: Int): Boolean

  def intersects(bV: BitVector): Int;

  def nbWords: Int

  def ^(bv: BitVector): BitVector

  def &(bv: BitVector): BitVector

  def |(bv: BitVector): BitVector

  def isEmpty: Boolean

  def cardinality: Int

  def getWord(i: Int): Long

  def subsetOf(bv: BitVector): Boolean

  def setWordExpand(pos: Int, word: Long): BitVector

  def setWordShrink(pos: Int, word: Long): BitVector

  override def filter(f: Int => Boolean): BitVector

  def filterBounds(f: Int => Boolean): BitVector

  def nextOrLoop(i: Int): Int = {
    val n = nextSetBit(i)
    if (n < 0) {
      nextSetBit(0)
    } else {
      n
    }
  }

  def shift(n: Int): BitVector = {
    assert(!isEmpty)

    if (n == 0) {
      this
    } else if (n > 0) {

      val wordShift = n / WORD_SIZE

      val words = new Array[Long](BitVector.word(lastSetBit + n) + 1)

      assert(nbWords + wordShift <= words.length, s"$n $toString $wordShift ${words.toSeq}")

      System.arraycopy(this.words, 0, words, wordShift, nbWords)

      if (n % WORD_SIZE != 0) {
        // Do the shift
        var i = words.length - 1
        while (i > wordShift) {
          words(i) <<= n; // Shift current word
          words(i) |= words(i - 1) >>> (WORD_SIZE - n); // Do the carry
          i -= 1
        }
        words(i) <<= n; // shift [words.length-1] separately, since no carry
      }
      BitVector(words)

    } else {

      val nn = -n

      val wordShift = nn / WORD_SIZE

      if (nbWords - wordShift <= 0) {
        EmptyBitVector
      } else {

        val words = new Array[Long](nbWords - wordShift)
        System.arraycopy(this.words, wordShift, words, 0, nbWords - wordShift)

        if (n % WORD_SIZE != 0) {
          // Do the shift
          var i = 0
          while (i < words.length - 1) {
            words(i) >>>= nn; // Shift current word
            words(i) |= words(i + 1) << (WORD_SIZE - nn); // Do the carry
            i += 1
          }
          words(i) >>>= nn;
        }

        BitVector(words)

      }
    }

  }

  override def toString: String =
    this.getClass.getSimpleName + iterator.mkString("{", ", ", "}")

  def iterator: Iterator[Int] = keysIteratorFrom(0)

  def keysIteratorFrom(start: Int): Iterator[Int] = new Iterator[Int] {
    private var current = nextSetBit(start)

    def hasNext: Boolean = current >= 0

    def next(): Int = {
      val c = current
      current = nextSetBit(current + 1)
      c
    }
  }

  implicit def ordering: Ordering[Int] = Ordering.Int

  def rangeImpl(from: Option[Int], until: Option[Int]): BitVector = {
    val low = from.map { lb => clearUntil(lb) }.getOrElse(this)
    until.map { ub => low.clearFrom(ub) }.getOrElse(low)
  }
}
