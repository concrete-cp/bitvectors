package bitvectors

import java.util.Arrays
import BitVector._

object LargeBitVector {

  def apply(words: Array[Long]) = {
    assert(words.length > 1, s"${Arrays.toString(words)} should use SmallBitVector")
    assert(words.last != 0L, s"${Arrays.toString(words)} should be shrinked")
    new LargeBitVector(words)
  }
}

class LargeBitVector private[bitvectors] (val words: Array[Long]) extends AnyVal with BitVector {

  //  assert(words.length > 1)
  //  assert(words(words.length - 1) != 0)
  def -(position: Int): BitVector = {
    val wordPos: Int = word(position)
    val oldWord = getWord(wordPos)
    val newWord = oldWord & ~(1L << position)
    if (oldWord == newWord) {
      this
    } else {
      setWordShrink(wordPos, newWord)
    }
  }

  def apply(position: Int): Boolean = {
    val wordPos = word(position)
    wordPos < nbWords && (words(wordPos) & (1L << position)) != 0L
  }

  def nextSetBit(start: Int): Int = {
    var position = word(start)

    if (position >= nbWords) {
      -1
    } else {
      var ctz = java.lang.Long.numberOfTrailingZeros(words(position) & (MASK << start))

      while (ctz >= WORD_SIZE) {
        position += 1
        if (position >= nbWords) {
          return -1
        }
        ctz = java.lang.Long.numberOfTrailingZeros(words(position))
      }
      position * WORD_SIZE + ctz
    }
  }

  def prevSetBit(start: Int): Int = {
    val wordsInUse = words.length
    val startWord = BitVector.word(start)
    var position: Int = math.min(wordsInUse - 1, startWord)

    var word = words(position);
    if (position == startWord) {
      word &= ~(MASK << start)
    }

    while (word == 0) {
      position -= 1
      if (position < 0) {
        return -1
      }
      word = words(position)
    }

    (1 + position) * WORD_SIZE - java.lang.Long.numberOfLeadingZeros(word) - 1
  }

  def prevClearBit(start: Int): Int = {
    val wordsInUse = words.length;
    val startWord = BitVector.word(start)
    var position = math.min(wordsInUse - 1, startWord);

    var word = ~words(position);
    if (position == startWord) {
      word &= ~(MASK << start);
    }

    while (word == 0) {
      position -= 1
      if (position < 0) {
        return -1;
      }
      word = ~words(position)
    }
    (1 + position) * WORD_SIZE - java.lang.Long.numberOfLeadingZeros(word) - 1;
  }

  def ^(bv: BitVector): BitVector = {
    val n = math.max(nbWords, bv.nbWords)

    val words = Array.ofDim[Long](n)
    var i = n - 1
    while (i >= 0) {
      words(i) = getWord(i) ^ bv.getWord(i);
      i -= 1
    }
    BitVector(words)
  }

  def &(bv: BitVector): BitVector = {
    val n = math.min(nbWords, bv.nbWords)
    val newWords = Array.ofDim[Long](n)
    var i = n - 1
    while (i >= 0) {
      newWords(i) = bv.getWord(i) & words(i);
      i -= 1
    }
    BitVector(newWords)
  }

  private def union(long: Array[Long], short: Array[Long]): Array[Long] = {
    val newWords = long.clone
    var i = short.length - 1
    while (i >= 0) {
      newWords(i) |= short(i)
      i -= 1
    }
    newWords
  }

  def |(bv: BitVector): BitVector = {
    val newWords =
      if (nbWords > bv.nbWords) {
        union(words, bv.words)
      } else {
        union(bv.words, words)
      }
    LargeBitVector(newWords)
  }

  def clearFrom(from: Int): BitVector = {
    if (from <= 0) {
      BitVector.empty
    } else {
      val startWordIndex = word(from)

      if (startWordIndex >= nbWords) {
        this
      } else if (startWordIndex == 0) {
        val newWord = words(0) & ~(MASK << from)
        if (newWord == 0L) {
          EmptyBitVector
        } else {
          new SmallBitVector(newWord)
        }
      } else {

        val lastWord = words(startWordIndex) & ~(MASK << from)

        if (lastWord == 0L) {
          // Just shrink array (avoid copying it twice)
          BitVector(words, startWordIndex)
        } else if (startWordIndex + 1 < nbWords || lastWord != words(startWordIndex)) {

          val newWords = Arrays.copyOf(words, startWordIndex + 1)
          newWords(startWordIndex) = lastWord
          LargeBitVector(newWords)

        } else {
          // No change
          this
        }
      }
    }
  }

  def clearUntil(until: Int): BitVector = {
    if (until < 0) {
      this
    } else {
      val endWordIndex = word(until);
      if (endWordIndex >= words.length) {
        BitVector.empty
      } else {
        val newWords = words.clone
        val w = newWords(endWordIndex)

        // Handle first word
        newWords(endWordIndex) &= (MASK << until);

        if (endWordIndex == words.length - 1 && newWords(endWordIndex) == 0L) {
          BitVector.empty
        } else {

          var removed = w != newWords(endWordIndex);

          // Handle intermediate words, if any
          var i = endWordIndex - 1
          while (i >= 0) {
            if (newWords(i) != 0) {
              newWords(i) = 0;
              removed = true;
            }
            i -= 1
          }

          if (removed) {
            LargeBitVector(newWords)
          } else {
            this
          }
        }
      }
    }
  }

  //  override def equals(o: Any): Boolean = o match {
  //    case bv: BitVector =>
  //      for (i <- 0 until nbWords) {
  //        if (getWord(i) != bv.getWord(i)) {
  //          return false;
  //        }
  //      }
  //      true;
  //    case _ => false
  //  }
  //
  //  override def hashCode(): Int = {
  //    var result = 721L;
  //    for (w <- words) {
  //      result = 31 * result + w;
  //    }
  //
  //    result.toInt;
  //  }

  def intersects(bv: BitVector, position: Int): Boolean = {
    (bv.getWord(position) & getWord(position)) != 0;
  }

  def intersects(bv: BitVector): Int = {
    val bvw = bv.words
    var i = math.min(bvw.length, words.length) - 1
    while (i >= 0) {
      if ((words(i) & bvw(i)) != 0) {
        return i;
      }
      i -= 1
    }

    -1;
  }

  def nbWords: Int = words.length

  def isEmpty: Boolean = {
    assert(words.last != 0L)
    false
  }

  def cardinality: Int = {
    var cardinality = 0;
    var i = words.length - 1
    while (i >= 0) {
      cardinality += java.lang.Long.bitCount(words(i));
      i -= 1
    }
    cardinality;
  }

  def subsetOf(bv: BitVector): Boolean = {
    for (i <- 0 until words.length) {
      if ((words(i) & ~bv.getWord(i)) != 0L) {
        return false;
      }
    }
    true;
  }

  def getWord(i: Int): Long = {
    if (i >= words.length)
      0L;
    else
      words(i);
  }

  def setWordExpand(pos: Int, word: Long): BitVector = {
    val newWords = Arrays.copyOf(words, math.max(words.length, pos + 1)) //, x$2)words.padTo(pos + 1, 0L)
    newWords(pos) = word;
    LargeBitVector(newWords)
  }

  def setWordShrink(pos: Int, word: Long): BitVector = {
    if (word == 0L && pos >= nbWords - 1) {
      var trimTo = pos - 1
      while (trimTo >= 0 && words(trimTo) == 0L) {
        trimTo -= 1
      }
      val newWords = Arrays.copyOf(words, trimTo + 1)
      newWords.length match {
        case 0 => EmptyBitVector
        case 1 => new SmallBitVector(newWords.head)
        case _ => LargeBitVector(newWords)
      }
    } else {
      val newWords = Arrays.copyOf(words, words.length) //, x$2)words.padTo(pos + 1, 0L)
      newWords(pos) = word;
      LargeBitVector(newWords)
    }
  }

  def filter(f: Int => Boolean): BitVector = {
    var words: Array[Long] = null //new Array[Long](nbWords)
    var i = nextSetBit(0)
    while (i >= 0) {
      if (!f(i)) {
        if (words == null) {
          words = this.words.clone()
        }
        words(word(i)) &= ~(1L << i)
      }
      i = nextSetBit(i + 1)
    }
    if (words == null) {
      this
    } else {
      BitVector(words)
    }
  }

  def filterBounds(f: Int => Boolean): BitVector = {
    val words = this.words.clone
    var i = nextSetBit(0)
    while (i >= 0 && !f(i)) {
      words(word(i)) &= ~(1L << i)
      i = nextSetBit(i + 1)
    }
    if (i >= 0) {
      var i = lastSetBit
      while (i >= 0 && !f(i)) {
        words(word(i)) &= ~(1L << i)
        i = prevSetBit(i)
      }
    }
    if (Arrays.equals(words, this.words)) {
      this
    } else {
      BitVector(words)
    }
  }

  def lastSetBit: Int = {
    val length = words.length
    val word = words(length - 1)
    return length * WORD_SIZE - java.lang.Long.numberOfLeadingZeros(word) - 1;
  }
}
