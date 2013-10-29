package patmat
import org.junit.Assert._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._
import junit.framework.Assert

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assertEquals(weight(t1), 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assertEquals(chars(t2), List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times: [] -> []") {
    assertEquals(times(List()), List())
  }

  test("times: ['a'] -> [{'a', 1}]") {
    val result = times(List('a'))
    println(result)
    assertEquals(result, List(('a', 1)))
  }

  test("times: ['a', 'b'] -> [{'a', 1}, {'b', 1}]") {
    val result = times(List('a', 'b'))
    println(result)
    assertEquals(result.toSet, List(('a', 1), ('b', 1)).toSet)
  }

  test("times: ['a', 'b', 'a'] -> [{'a', 2}, {'b', 1}]") {
    val result = times(List('a', 'b', 'a'))
    println(result)
    assertEquals(result.toSet, List(('a', 2), ('b', 1)).toSet)
  }

  test("makeOrderedLeafList: [] -> []") {
    val result = makeOrderedLeafList(List())
    println(result)
    assertEquals(result, List())
  }

  test("makeOrderedLeafList: [{'e', 1}] -> [{e1}]") {
    val result = makeOrderedLeafList(List(('e', 1)))
    println(result)
    assertEquals(result, List(Leaf('e', 1)))
  }

  test("makeOrderedLeafList: [{'t', 2}, {'e', 1}, {'x', 3}] -> [{e1}, {t2}, {x3}]") {
    val result = makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
    println(result)
    assertEquals(result, List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton: [] -> false") {
    assert(!singleton(List()))
  }

  test("singleton: [t1] -> true") {
    assert(singleton(List(Leaf('a', 1))))
  }

  test("singleton: [t1, t2] -> true") {
    assert(!singleton(List(Leaf('a', 1), Leaf('b', 2))))
  }

  test("combine: [] => []") {
    val leaflist = List()
    val result = combine(leaflist);
    println(result)
    assertEquals(result, List())
  }

  test("combine: [{e1}] -> [{e1}]") {
    val leaflist = List(Leaf('e', 1))
    val result = combine(leaflist);
    println(result)
    assertEquals(result, List(Leaf('e', 1)))
  }
  
  
  test("combine: [{e1},{t2}] -> [{e1}{{et3}{t2}]") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    val result = combine(leaflist);
    println(result)
    assertEquals(result, List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))
  }


  test("combine: [{e1}, {t2}, {x4}] -> [{e1}{{et3}{t2}, {x4}]") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val result = combine(leaflist);
    println(result)
    assertEquals(result, List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }
  
  test("until: done: x => x > 5, transform: x => x + 1, it: 1 -> 6") {
    def done(x: Int) = x > 5
    def transform(x: Int) = x + 1
    assertEquals(until(done, transform)(1), 6)
  }
  
  test("until: done: x => x > 5, transform: x => x + 1, it: 6 -> 6") {
    def done(x: Int) = x > 5
    def transform(x: Int) = x + 1
    assertEquals(until(done, transform)(6), 6)
  }
  
  test("createCodeTree: [a] -> {a, 1}") {
    val result = createCodeTree(List('a'))
    println(result)
    
    assert(result == Leaf('a', 1))
    
  }
  
  test("createCodeTree: [a, b] -> {{a1}{ab2}{b1}}") {
    val result = createCodeTree(List('a', 'b'))
    println(result)
    
    assert(result == Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2))
    
  }
  
  test("createCodeTree: [a, b, a] -> {{b1}{ab3}{a2}}") {
    val result = createCodeTree(List('a', 'b', 'a'))
    println(result)
    
    assert(result == Fork(Leaf('b', 1), Leaf('a', 2), List('b', 'a'), 3))
    
  }
  
  test("createCodeTree: [a, b, a, c] -> {{a2}{abc4}{{b1}{bc2}{c1}}") {
    val result = createCodeTree(List('a', 'b', 'a', 'c'))
    println(result)
    
    assert(result == Fork(Leaf('a',2),Fork(Leaf('b',1),Leaf('c',1),List('b', 'c'),2),List('a', 'b', 'c'),4))
    
  }
  
  test("print french secret") {
    println(decodedSecret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
    }
  }
  
  test("code bits: empty table with 'a' returns Nil") {
    assertEquals(codeBits(List())('a'), Nil)
  }
  
  test("code bits: [{a -> 0100}] with 'a' returns [0100]") {
    assertEquals(codeBits(List(('a', List(0, 1, 0, 0))))('a'), List(0, 1, 0, 0))
  }
  
  test("code bits: [{a -> 0100}, {b -> 1100}] with 'b' returns [1100]") {
    assertEquals(codeBits(List(('a', List(0, 1, 0, 0)), ('b', List(1, 1, 0, 0))))('b'), List(1, 1, 0, 0))
  }
  
  test("code table") {
    val codeTable = convert(frenchCode)
    def f(pair: (Char, List[Bit])) = {
      val decoded = decode(frenchCode, pair._2)
      assert(decoded.head == pair._1)
    }
    codeTable.foreach(f)
  }
  
  test("quickEncode is same as encode") {
    val e = encode(frenchCode)("huffmanestcool".toList)
    val q = quickEncode(frenchCode)("huffmanestcool".toList)
    
    println("encode: " + e)
    println("quickEncode: " + q)
    
    assertEquals(e, q)
  }
}
