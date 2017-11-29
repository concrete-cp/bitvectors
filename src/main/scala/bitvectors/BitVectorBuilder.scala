package bitvectors

import java.util

import bitvectors.BitVector.{MASK, WORD_SIZE, word}

class BitVectorBuilder(
                        var words: Array[Long],
                        var nbWords: Int
                      ) extends Iterable[Int] {

  var change = false

  def this(ubHint: Int) = this(new Array(BitVector.word(ubHint) + 1), 0)

  def this() = this(new Array(16), 0)

  def +=(position: Int): Unit = {
    val wordPos = BitVector.word(position)
    ensureCapacity(wordPos)
    val nw = words(wordPos) | (1L << position)
    if (nw != words(wordPos)) {
      change = true
      words(wordPos) = nw
      nbWords = math.max(nbWords, wordPos + 1)
    }
  }

  def set(from: Int, until: Int): Unit = {
    if (from < until) {
      val startWordIndex = word(from)
      val maskFrom = MASK << from
      val lastWordIndex = word(until - 1)
      val maskTo = MASK >>> -until

      ensureCapacity(lastWordIndex)
      nbWords = math.max(nbWords, lastWordIndex + 1)
      val sw = words(startWordIndex)

      if (startWordIndex == lastWordIndex) {
        words(startWordIndex) |= (maskFrom & maskTo)
      } else {
        words(startWordIndex) |= maskFrom

        val newWord = words(lastWordIndex) | maskTo

        if (newWord != words(lastWordIndex)) {
          change = true
          words(lastWordIndex) = newWord
        }

        for (i <- startWordIndex + 1 until lastWordIndex) {
          if (words(i) != MASK) {
            words(i) = MASK
            change = true
          }
        }

      }
      change |= (sw != words(startWordIndex))
    }
  }

  /** Ensures that words(w) is available */
  private def ensureCapacity(w: Int): Unit = {
    if (w >= words.length) {
      var newSize = math.max(words.length * 2, 16)
      while (w >= newSize) {
        newSize *= 2
      }
      words = util.Arrays.copyOf(words, newSize)
    }
  }

  def result(): BitVector = {
    val r = BitVector.apply(words, nbWords)
    words = null
    r
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    private var current = nextSetBit(0)

    def hasNext: Boolean = current >= 0

    def next(): Int = {
      val c = current
      current = nextSetBit(current + 1)
      c
    }
  }

  def nextSetBit(start: Int): Int = {
    var u = word(start)

    if (u >= nbWords) {
      -1
    } else {
      var word = words(u) & (MASK << start)

      while (true) {
        if (word != 0) {
          return (u * WORD_SIZE) + java.lang.Long.numberOfTrailingZeros(word)
        }
        u += 1
        if (u == nbWords) {
          return -1
        }
        word = words(u)
      }
      throw new IllegalStateException()
    }
  }

}
