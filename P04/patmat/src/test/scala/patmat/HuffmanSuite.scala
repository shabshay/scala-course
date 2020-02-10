package patmat

import org.junit.Assert.assertEquals
import org.junit._

class HuffmanSuite {

  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  @Test def `make code tree weight`: Unit = {
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    )
    assertEquals(4, sampleTree.weight)
  }

  @Test def `times map`: Unit = {
    val timesMap = times(List('a', 'b', 'a'))
    assert(timesMap.contains(('a', 2)))
  }

  @Test def `make Ordered Leaf List`: Unit = {
    val list: List[(Char, Int)] = List(('a', 30), ('b', 6), ('d', 20), ('e', 1))
    val orderedList = makeOrderedLeafList(list)
    assertEquals('e', orderedList.head.char)
    assertEquals('a', orderedList.reverse.head.char)
  }

  @Test def `combine trees list`: Unit = {
    val list: List[CodeTree] = combine(List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4), Leaf('d', 6)))
    assertEquals(3, list.size)
    assertEquals('c', list.head match{
      case Leaf(char, _) => char
      case _ => 'x'
    })
  }

  @Test def `until done`: Unit = {
    val list: List[CodeTree] = List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4), Leaf('d', 1))
    val root = until(singleton, combine)(list)
    assertEquals(1, root.size)
    assertEquals(10, root.head match{
      case Fork(_, _, _, weight) => weight
      case _ => 0
    })
  }

  @Test def `create code tree`: Unit = {
    val list: List[Char] = List('a', 'b','c', 'c','d')
    val root = createCodeTree(list)
    assertEquals(5, weight(root))
  }

  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a', 'b', 'd'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leaflist))
  }


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
