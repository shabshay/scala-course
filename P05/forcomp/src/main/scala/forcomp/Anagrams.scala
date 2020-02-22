package forcomp

object Anagrams extends AnagramsInterface {

  type Occurrence = (Char, Int)

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[Occurrence]

  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences = {
    val charArray = w.toLowerCase.toCharArray
    val grouped: Set[(Char, Array[Char])] = charArray.groupBy(c => c).toSet
    val occ = grouped.map({ case (a, b) => (a, b.length) }).toList.sorted
    occ
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val occ: Occurrences = wordOccurrences(s.mkString)
    occ
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val grouped: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))
    grouped
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val words = dictionaryByOccurrences.get(wordOccurrences(word))
    words.getOrElse(List.empty)
  }

  def getCharCombs(char: Char, count: Int): List[Occurrences] = {
    count match {
      case 0 => List(List())
      case i if i > 0 => List((char, i)) :: getCharCombs(char, count - 1)
    }
  }

  def withRestCombs(oc: Occurrences, restCombs: List[Occurrences]): List[Occurrences] = {
    val ret = restCombs.map(occs => oc ::: occs)
    ret
  }

  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    occurrences match {

      case List() => List(List.empty)

      case (char, count) :: rest =>

        val restCombs: List[Occurrences] = combinations(rest)

        val charOccCombs: List[Occurrences] = getCharCombs(char, count)

        val combs: List[Occurrences] = charOccCombs.flatMap(oc => withRestCombs(oc, restCombs))
        combs
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val subtracted: Occurrences = x.map(xOcc => {
      val yOcc: Option[(Char, Int)] = y.find(occ => occ._1 == xOcc._1)

      if (yOcc.isDefined) {
        (xOcc._1, xOcc._2 - yOcc.get._2)
      } else {
        xOcc
      }
    })

    subtracted.filter(occ => occ._2 > 0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams2(sentence: Sentence): List[Sentence] = {

    def sentenceAnagrams(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) {
        List(List.empty)
      }
      else {
        val occurrencesCombinations: List[Occurrences] = combinations(occurrences)

        val allSentences: List[Sentence] = occurrencesCombinations.flatMap(
          (occ: Occurrences) => {

            val occAnagrams: Option[List[Word]] = dictionaryByOccurrences.get(occ)

            if (occAnagrams.isDefined) {

              val subOccurrences: Occurrences = subtract(occurrences, occ)

              if (subOccurrences.nonEmpty) {
                val subSentences: List[Sentence] = sentenceAnagrams(subOccurrences)

                if (subSentences.flatten.nonEmpty) {

                  val sentences: List[Sentence] = subSentences.map(
                    sentence =>
                      occAnagrams.get.flatMap((word: Word) => {
                        if (sentence.nonEmpty) {
                          val allSentence = word :: sentence
                          allSentence
                        }
                        else {
                          List.empty
                        }
                      }))

                  sentences
                }
                else {
                  val angaramsSentences: List[Sentence] = occAnagrams.get.map(word => List(word))
                  angaramsSentences
                }
              }
              else {
                val angaramsSentences: List[Sentence] = occAnagrams.get.map(word => List(word))
                angaramsSentences
              }
            } else {
              List(List.empty)
            }
          })

        for (s <- allSentences.distinct) {
          if (sentenceOccurrences(s) != occurrences) {
            val x = true
          }
        }
        allSentences.distinct

        val filteredSentences = allSentences.filter(s => sentenceOccurrences(s) == occurrences).distinct
        if (filteredSentences.nonEmpty)
          filteredSentences
        else
          List(List.empty)
      }
    }

    val occurrences: Occurrences = sentenceOccurrences(sentence)
    sentenceAnagrams(occurrences)
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    def sentenceAnagrams(occurrences: Occurrences): List[Sentence] = {

      if (occurrences.isEmpty) {
        return List(List.empty)
      }

      //
      if (occurrences.size == 2){
        val b = true
      }
      //

      var ret: List[Sentence] = List()

      val occurrencesCombinations: List[Occurrences] = combinations(occurrences)

      for (comb <- occurrencesCombinations) {
        val anagrams: List[Word] = dictionaryByOccurrences.getOrElse(comb, List.empty)

        if (anagrams.flatten.nonEmpty) {

          val subOccurrences: Occurrences = subtract(occurrences, comb)

          if (subOccurrences.isEmpty){
            ret = anagrams.map(word => List(word)) ::: ret
          }

          val subOccSentences: List[Sentence] = sentenceAnagrams(subOccurrences)

          if (subOccSentences.flatten.nonEmpty) {
            for (anagram <- anagrams) {
              for (subSentence <- subOccSentences) {
                val sentence: Sentence = anagram :: subSentence
                ret = sentence :: ret
              }
            }
          }
        }
      }

      ret
    }

    val occurrences = sentenceOccurrences(sentence)
    sentenceAnagrams(occurrences)
  }

  //  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  //
  //    def sentenceAnagrams(occurrences: Occurrences): List[Sentence] = {
  //      if (occurrences.isEmpty) {
  //        List(List.empty)
  //      }
  //      else {
  //        val occurrencesCombinations: List[Occurrences] = combinations(occurrences)
  //
  //        val sentences: List[Sentence] = occurrencesCombinations.flatMap(occ => {
  //          val occurenceSentences = buildOccurrenceSentences(occ, occurrences)
  //          occurenceSentences
  //        })
  //
  //        sentences.sortBy(s => s)
  //      }
  //    }
  //
  //    def buildOccurrenceSentences(subOcc: Occurrences, occurrences: Occurrences): List[Sentence] = {
  //      if (subOcc.isEmpty) {
  //        List(List.empty)
  //      }
  //      else {
  //        val occAnagrams: Option[List[Word]] = dictionaryByOccurrences.get(subOcc)
  //
  //        if (occAnagrams.isDefined) {
  //          val subOccs: Occurrences = subtract(occurrences, subOcc)
  //          val subSentenceAnagrams: List[Sentence] = sentenceAnagrams(subOccs)
  //          if (subSentenceAnagrams.isEmpty)
  //            List(List.empty)
  //          else {
  //            val wordToSentences: List[List[Sentence]] = occAnagrams.get.filter(word => word.nonEmpty).map(
  //              word =>
  //                subSentenceAnagrams.map(sentence =>
  //                  word :: sentence
  //                )
  //            )
  //            wordToSentences.flatten
  //          }
  //        }
  //        else {
  //          List(List.empty)
  //        }
  //      }
  //    }
  //
  //    val occurrences = sentenceOccurrences(sentence)
  //    sentenceAnagrams(occurrences)
  //  }

}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
