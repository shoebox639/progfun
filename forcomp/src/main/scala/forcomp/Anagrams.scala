package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences =
    w.groupBy(c => c.toLower).map { case (k, v) => (k, v.length) }.toList sortBy (_._1)

  def dedupe(o: Occurrences): Occurrences = o.groupBy(x => x._1).map {
    case (k, v) => (k, v.map { _._2 }.sum)
  }.toList sortBy (_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    def combine(o1: Occurrences, o2: Occurrences): Occurrences = dedupe(o1 ::: o2)
    (for (w <- s) yield wordOccurrences(w)).foldRight[Occurrences](List())(combine)
  }

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    (for (w <- dictionary) yield (wordOccurrences(w) -> w)).groupBy { case (k, v) => k }.map { case (k, v) => (k, v.map(_._2)) }
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.get(wordOccurrences(word)).getOrElse(List())

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    //        def expand(next: (Char, Int), acc: Occurrences): Occurrences = {
    //          val (char, count) = next
    //          val e = (for (i <- 1 to count) yield (char, 1)).toList ::: acc
    //          e
    //        }
    //        val expanded = occurrences.foldRight[Occurrences](List())(expand)
    //    
    //    
    //    def addNext(next: (Char, Int), acc: List[Occurrences]): List[Occurrences] = {
    //      println(acc)
    //      (for (e <- acc) yield dedupe(next :: e)) ::: acc       
    //    }

    def addNext(next: (Char, Int), acc: List[Occurrences]): List[Occurrences] = next match {
      case (char, 0) => acc
      case (char, count) => {
        def removeChar(o: Occurrences): Occurrences = {
          val (has, doesntHave) = o.partition(_._1 == char)
          val hasRemoved = has match {
            case Nil => Nil
            case head :: tail => head match {
              case (_, 1) => Nil
              case (c, count) => List((c, count - 1))
            }
          }
          dedupe(hasRemoved ::: doesntHave).sortBy(_._1)
        }

        addNext((char, count - 1), (acc.map(removeChar) ::: acc)).distinct
      }
    }

    occurrences.foldRight[List[Occurrences]](List(occurrences))(addNext)
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def sub(x: (Char, Int)) = {
      val (char, count) = x
      (char, count - y.find(_._1 == char).map(_._2).getOrElse(0))
    }

    x.map(sub).filter(_._2 > 0)
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def helper(o: Occurrences, acc: Sentence): List[Sentence] = {
      val allCombos = combinations(o)
      (for {
        combo: Occurrences <- allCombos
        word: Word <- dictionaryByOccurrences.getOrElse(combo, List())
      } yield {
        val leftOver = subtract(o, combo)
        if (leftOver.isEmpty) List((word :: acc))
    	else helper(leftOver, word :: acc)
      }).flatten[Sentence]
    }

    helper(sentenceOccurrences(sentence), List()) match {
      case Nil => List(Nil)
      case a => a
    }
  }
}