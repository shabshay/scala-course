package forcomp

import org.junit.Assert.assertEquals
import org.junit._


class AnagramsSuite {

  import Anagrams._

  @Test def `wordOccurrences: abcd (3pts)`: Unit =
    assertEquals(List(('a', 1), ('b', 1), ('c', 1), ('d', 1)), wordOccurrences("abcd"))

  @Test def `wordOccurrences: Robert (3pts)`: Unit =
    assertEquals(List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)), wordOccurrences("Robert"))


  @Test def `sentenceOccurrences: abcd e (5pts)`: Unit =
    assertEquals(List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)), sentenceOccurrences(List("abcd", "e")))

  @Test def `sentenceOccurrences: i love linuex (5pts)`: Unit =
    assertEquals(List(('e',1), ('i',2), ('l',2), ('n',1), ('o',1), ('u',1), ('v',1), ('x',1)), sentenceOccurrences(List("I", "love", "Linux")))


  @Test def `dictionaryByOccurrences.get: eat (10pts)`: Unit =
    assertEquals(Some(Set("ate", "eat", "tea")), dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet))


  @Test def `wordAnagrams married (2pts)`: Unit =
    assertEquals(Set("married", "admirer"), wordAnagrams("married").toSet)

  @Test def `wordAnagrams player (2pts)`: Unit =
    assertEquals(Set("parley", "pearly", "player", "replay"), wordAnagrams("player").toSet)


  @Test def `subtract: lard - r (10pts)`: Unit = {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(lad, subtract(lard, r))
  }

  @Test def `subtract: linuxrulesz - linux (10pts)`: Unit = {
    val occs = List(('i',1), ('l',1), ('n',1), ('u',1), ('x',1), ('e',1), ('l',1), ('r',1), ('u',1), ('z',1))
    val subOcc = List(('i',1), ('l',1), ('n',1), ('u',1), ('x',1))
    val expected = List(('e',1), ('r',1), ('z',1))
    assertEquals(expected, subtract(occs, subOcc))
  }


  @Test def `combinations: [] (8pts)`: Unit =
    assertEquals(List(Nil), combinations(Nil))

  @Test def `combinations: abba (8pts)`: Unit = {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    val combAbba = combinations(abba).toSet
    assertEquals(abbacomb.toSet, combAbba)
  }


  @Test def `sentence anagrams: [] (10pts)`: Unit = {
    val sentence = List()
    val res = sentenceAnagrams(sentence)
    assertEquals(List(Nil), res)
  }

  @Test def `sentence anagrams: Linux rulez (10pts)`: Unit = {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    val res = sentenceAnagrams(sentence).toSet
    assertEquals(anas.toSet, res)
  }

  @Test def `sentence anagrams: yes man`: Unit = {
    val sentence = List("Yes", "man")
    val anas = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )
    val res = sentenceAnagrams(sentence).toSet
    val dif = anas.diff(res.toList)
    assertEquals(anas.toSet, res)
  }

  @Test def `sentence anagrams: emacs`: Unit = {
    val sentence = List("emacs")
    val anas = List(
      List("emacs"), List("maces")
    )
    val res = sentenceAnagrams(sentence).toSet
    assertEquals(anas.toSet, res)
  }

  @Test def `sentence anagrams: my as en`: Unit = {
    val sentence = List("my", "as", "en")
    val anas = List(
      List("my", "sane"),
      List("as", "my", "en"),
      List("Sean", "my"),
      List("my", "as", "en"),
      List("my", "en", "as"),
      List("yes", "man"),
      List("my", "Sean"),
      List("en", "my", "as"),
      List("en", "as", "my"),
      List("men", "say"),
      List("as", "en", "my"),
      List("say", "men"),
      List("man", "yes"),
      List("sane", "my")
    )
    val res = sentenceAnagrams(sentence).toSet
    assertEquals(anas.toSet, res)
  }

  @Test def `sentence anagrams: as en`: Unit = {
    val sentence = List("as", "en")
    val anas = List(
      List("sane"), List("Sean"), List("as", "en"), List("en", "as")
    )
    val res = sentenceAnagrams(sentence).toSet
    assertEquals(anas.toSet, res)
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
