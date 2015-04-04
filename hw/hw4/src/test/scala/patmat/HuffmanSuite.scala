package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of a larger tree") {
    new TestTrees {
      assert(times(List('a', 'a', 'b', 'c', 'b', 'c', 'a', 'd', 'd')) === List( ('a', 3), ('b', 2), ('c', 2), ('d', 2) ) )
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until on a List of CodeTrees") {
    new TestTrees {
      val treeList = List(t1, t2)
      val untilSingle = until(singleton, combine)(_)
      println (untilSingle(treeList))
    }
  }

  test("French Code") {
    println(Huffman.decodedSecret)
  }

  test("Encode 'bonjour'") {
    val code = encode(Huffman.frenchCode)(List('b', 'o', 'n', 'j', 'o', 'u', 'r'))
    println(decode(Huffman.frenchCode, code))
  }

  test("codeBits on small table") {
    type Bit = Int
    type CodeTable = List[(Char, List[Bit])]
    val table:CodeTable = List( ('a', List(0, 0)), ('b', List(0,1)), ('c', List(1)) )
    println(codeBits(table)('b'))
  }

  test("Convert small table") {
    new TestTrees {
      println(convert(t2))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
