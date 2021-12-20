//package patmat

import patmat.{Fork, Leaf}

class HuffmanSuite extends munit.FunSuite {

  import patmat.Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(weight(t1), 5)
    }
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(chars(t2), List('a', 'b', 'd'))
    }
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("char times") {
    assertEquals(times(List('a', 'b', 'a', 'c', 'c', 'a')), List(Tuple2(_1 = 'a', _2 = 3), Tuple2(_1 = 'b', _2 = 1), Tuple2(_1 = 'c', _2 = 2)))
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    //assertEquals(makeOrderedLeafList(List(('e', 3), ('t', 2), ('x', 1))), List(Leaf('x',1), Leaf('t',2), Leaf('e',3)))
    assertEquals(makeOrderedLeafList(List(('t', 4), ('e', 1), ('x', 3))), List(Leaf('e', 1), Leaf('x', 3), Leaf('t', 4)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('a', 5))
    assertEquals(combine(leaflist), List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7), Leaf('a', 5)))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist2 = List()
    assertEquals(combine(leaflist2), List())
  }

  test("create tree") {
    val createTree = List('a', 'a', 'b', 'c', 'b', 'a', 'd')
    //assertEquals(createCodeTree(createTree), Fork(Fork(Leaf('a', 3), Leaf('c', 1), List('a', 'c'), 4), Leaf('b', 2), List('a', 'c', 'b'), 6))
    assertEquals(createCodeTree(createTree),
      Fork(
        Leaf('a', 3),
        Fork(
          Leaf('b', 2),
          Fork(
            Leaf('c', 1),
            Leaf('d', 1),
            List('c', 'd'), 2),
          List('b', 'c', 'd'), 4),
        List('a', 'b', 'c', 'd'), 7)
    )
  }

  test("singleton") {
    val createTree = List('a', 'a', 'b', 'c', 'b', 'a')
    assertEquals(singleton(List(createCodeTree(createTree))), true)
  }

  test("char times") {
    val tree = List('a', 'a', 'b', 'c', 'c', 'c', 'c', 'b', 'a', 'd', 'd', 'd', 'd', 'd', 'p', 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'o', 'o', 'o', 'o', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'l', 'l', 'l', 'k', 'j', 'h')
    assertEquals(times(tree), List(('a', 3), ('b', 2), ('c', 4), ('d', 5), ('e', 9), ('h', 1), ('j', 1), ('k', 1), ('l', 3), ('o', 4), ('p', 1), ('z', 7)))
  }

  test("create tree3") {
    val createTree = List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    assertEquals(createCodeTree(createTree),
      Fork(
        Leaf('a', 8),
        Fork(
          Fork(
            Fork(
              Leaf('c', 1),
              Leaf('d', 1),
              List('c', 'd'), 2),
            Fork(
              Leaf('e', 1),
              Leaf('f', 1),
              List('e', 'f'), 2),
            List('c', 'd', 'e', 'f'), 4),
          Fork(
            Fork(
              Leaf('g', 1),
              Leaf('h', 1),
              List('g', 'h'), 2),
            Leaf('b', 3),
            List('g', 'h', 'b'), 5),
          List('c', 'd', 'e', 'f', 'g', 'h', 'b'), 9),
        List('a', 'c', 'd', 'e', 'f', 'g', 'h', 'b'), 17))
  }

  test("decode") {
    val tree = createCodeTree(List('a', 'a', 'b', 'b', 'c', 'a', 'd'))
    assertEquals(decode(tree, List(0, 1, 0, 1, 1, 0, 1, 1, 1)), List('a', 'b', 'c', 'd'))
  }

  test("encode") {
    val tree = createCodeTree(List('a', 'a', 'a', 'b', 'c', 'c'))
    /*
    if you want to go down the left branch, pass 0 on the right - 1
    Tree:

       /  \
      a   b,c
          /  \
         c    b*/
    assertEquals(encode(tree)(List('c')), List(1, 1))
  }

  test("encode") {
    val tree = (List('a', 'a', 'a', 'b', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e'))

    /*
    Tree:

                /   \
            bea5      cd9
            /  \      / \
         be2   a3    c4  d5
        / \
       b1   e1
    */
    assertEquals(encode(createCodeTree(tree))(List('e')), List(0, 0, 1))
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees {
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
    }
  }


  import scala.concurrent.duration._

  override val munitTimeout = 10.seconds
}
