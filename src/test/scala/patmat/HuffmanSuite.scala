package patmat

class HuffmanSuite extends munit.FunSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(weight(t1), 5)
    }
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(chars(t2), List('a','b','d'))
    }
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("char times") {
    assertEquals(times(List('a','b','a','c','c', 'a')), List(Tuple2(_1 = 'a', _2 = 3),Tuple2(_1 = 'b', _2 = 1),Tuple2(_1 = 'c', _2 = 2)))
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    //assertEquals(makeOrderedLeafList(List(('e', 3), ('t', 2), ('x', 1))), List(Leaf('x',1), Leaf('t',2), Leaf('e',3)))
    assertEquals(makeOrderedLeafList(List(('t', 4), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('x',3), Leaf('t',4)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('a', 5))
    assertEquals(combine(leaflist), List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7), Leaf('a',5)))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist2 = List()
    assertEquals(combine(leaflist2), List())
  }

  test("create tree") {
    val createTree = List('a','a','b','c','b','a')
    assertEquals(createCodeTree(createTree), Fork(Fork(Leaf('c',1),Leaf('b',2),List('c', 'b'),3), Leaf('a',3), List('c','b','a'), 6))
  }

  test("decode") {
    //val tree = createCodeTree(List('a','a','b','c','c','c','c','b','a','d','d','d','d','d','e'))
    val tree = createCodeTree(List('a','a','b','c','c','a'))
    assertEquals(decode(tree,List(0,0,1,0,1)), List('b','a','c'))
  }

  test("encode") {
    //val tree = createCodeTree(List('a','a','b','c','c','c','c','b','a','d','d','d','d','d','e'))
    val tree = createCodeTree(List('a','a','b','c','c','a'))
    assertEquals(encode(tree)(List('b','a','c')),List(0,0,1,0,1))
  }

  test("encode") {
    val tree = createCodeTree(List('a','a','b','c','c','c','c','b','a','d','d','d','d','d','e'))
    //val tree = createCodeTree(List('a','a','b','c','c','a'))
    assertEquals(encode(tree)(List('d','e','c')),List(1,0,0,0,0,0,1))
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees {
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
    }
  }


  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}
