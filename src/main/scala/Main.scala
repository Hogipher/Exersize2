@main def Main : Unit = {
  def userName = "Unbothered1"
  def name = "Christopher Emmanuelle Hogue"
  def xNumber = "03099"

  def alternate(list1: List[Int], list2: List[Int]): List[Int] = {
    val alternatedLst = List.empty[Int]
    val reversedLst = List.empty[Int]
    def combine(list1: List[Int], list2: List[Int], alternatedLst: List[Int]):
    List[Int] = (list1,list2)
    match {
      case (Nil,Nil) => alternatedLst
      case (Nil,list2) => combine(list1,list2.tail,list2.head::alternatedLst)
      case (list1,Nil) => combine(list1.tail,list2,list1.head::alternatedLst)
      case (list1,list2) => combine(list1.tail,list2.tail,list2.head::(list1.head::alternatedLst))
      }
    def reverse(reversedLst: List[Int],list: List[Int]):List[Int] = list match{
      case Nil => reversedLst
      //reverse(list.head::reversedLst,list.tail)
      case head::tail => reverse(head::reversedLst,tail)
    }
    reverse(reversedLst,combine(list1, list2, alternatedLst))
  }
  val testLst0:List[Int] = List(11, 12, 13, 14)
  val testLst1:List[Int] = List(1, 2, 3, 4)
  println("RESULTS+++++++++++++++++++++++++++++++++++++++++++++++++RESULTS")
  println("test def alternate: " + alternate(testLst0,testLst1))
  println("--------------------")
  //IGNORE test("alternate", alternate _, "list1", "list2")

  def unzip(pairs: List[(String, Int)]): (List[String], List[Int]) = pairs match {
    case Nil => (List.empty[String], List.empty[Int])
    case head::tail => head match {
      case (str,int) => unzip(tail) match {
        case (strLst,intLst) => (str::strLst,int::intLst)
      }
    }
  }
  val pairs:List[(String,Int)] = List(("a",0),("b",1),("c",2))
  println("test def unzip: " + unzip(pairs))
  //IGNORE test("unzip", unzip _, "pairs")

  def cleanup(opts: List[Option[Int]]): List[Int] = {
    def sweep(cleanedLst:List[Int], opts: List[Option[Int]]):List[Int]= opts match {
      case Nil => cleanedLst
      case head::tail => head match {
        case None => sweep(cleanedLst,tail)
        case Some(int) => int::sweep(cleanedLst,tail)
      }
    }
    sweep(List.empty[Int],opts)
  }
  val dirtyLst:List[Option[Int]] = List(Some(1),None,Some(2),None,Some(3))
  println("--------------------")
  println("dirtyLst = " + dirtyLst)
  println("--------------------")
  println("test cleanup: " + cleanup(dirtyLst))
  println("--------------------                          SEARCHTREE PRACTICE")
  //IGNORE test("cleanup", cleanup _, "opts")

    /*DONTUNCOMMENT def unzip(pairs: List[(String, Int)]): (List[String], List[Int]) = {
      val numLst = List.empty[Int]
      val strLst = List.empty[String]
      val reversedInt = List.empty[Int]
      val reversedStr = List.empty[String]
      def unzipper(numLst:List[Int], strLst:List[String],pairs: List[(String, Int)],
        reversedInt:List[Int], reversedStr:List[String]):(List[String], List[Int]) = pairs match {
        case Nil => (reverseStr(reversedStr,strLst),reverseInt(reversedInt,numLst))
        case pairs => pairs.head match {
          case (str,int) => unzipper(int::numLst,str::strLst,pairs.tail,reversedInt,reversedStr)
        }
          def reverseInt(reversedInt: List[Int],list: List[Int]):List[Int] = list match{
      case Nil => reversedInt
      case list => reverseInt(list.head::reversedInt,list.tail)
    }
    def reverseStr(reversedStr: List[String],list: List[String]):List[String] = list match{
      case Nil => reversedStr
      case list => reverseStr(list.head::reversedStr,list.tail)
    }
    unzipper(numLst,strLst,pairs,reversedInt,reversedStr)
  }
    }*/

  //TREE DEFINITION

  abstract sealed class SearchTree[+A ] {
    /**
     * The value of this tree.
     */
    def value: A

    /**
     * The left child of this tree.
     */
    def left: SearchTree[A]

    /**
     * The right child of this tree.
     */
    def right: SearchTree[A]

    /**
     * The size of this tree.
     */
    //def size: Int

    /**
     * Checks whether this tree is empty or not.
     */

     /**
    * Fails with given message 'm'.
    */
    def fail(m: String) = throw new NoSuchElementException(m)
  }

  case object Empty extends SearchTree[Nothing] {
    def value: Nothing = fail("An empty tree.")
    def left: SearchTree[Nothing] = fail("An empty tree.")
    def right: SearchTree[Nothing] = fail("An empty tree.")
    //def size: Int = 0

    def isEmpty: Boolean = true
  }
  object SearchTree {

    /**
     * An empty tree.
     */
    def empty[A]: SearchTree[A] = Empty

    /**
     * A smart constructor for tree's branch.
     *
     * Time - O(1)
     * Space - O(1)
     */
    def make[A <: Ordered[A]](left: SearchTree[A] = Empty, v: A, right: SearchTree[A] = Empty): SearchTree[A] =
      Node(left, v, right)
  }

  case class Node[A](left: SearchTree[A],
                                   value: A,
                                   right: SearchTree[A],
                                   ) extends SearchTree[A] {
                                   def isEmpty: Boolean = false
  }
  //TREE DEFINITION END
  import scala.collection.mutable.ArrayBuffer
  def zigzag(tree: SearchTree[Int]): String = {
    var array = new ArrayBuffer[List[Int]]()
    array += List[Int]();array += List[Int]();array += List[Int]();array += List[Int]();
    val lvl: Int = 0
    val dir: Boolean = false
    def traverse(tree: SearchTree[Int],lvl: Int): SearchTree[Int] = tree match {
      case Empty => tree
      case Node(Empty,y,Empty) => {array(lvl) = y::array(lvl);tree}
      case Node(lft,y,Empty) => {array(lvl) = y::array(lvl);Node(Empty,y,traverse(lft,lvl+1))}
      case Node(Empty,y,rgt) => {array(lvl) = y::array(lvl);Node(Empty,y,traverse(rgt,lvl+1))}
      case Node(lft,y,rgt)=> {array(lvl) = y::array(lvl);Node(traverse(lft,lvl+1),y,traverse(rgt,lvl+1))}
    }

    def zigZagger(): String = {
      val reversedLst = List.empty[Int]
      var index: Int = 0
      for (lst <- array){
        if index%2 == 0 then array(index) = reverse(reversedLst,lst)
        index = index + 1
      }
      def reverse(reversedLst: List[Int],list: List[Int]):List[Int] = list match{
        case Nil => reversedLst
        //reverse(list.head::reversedLst,list.tail)
        case head::tail => reverse(head::reversedLst,tail)
      }
      return "finished zagging"
    }
    traverse(tree,lvl)
    zigZagger()
    println("ZigZag Array = " + array)
    return "Finished"
  }

  //IGNORE test("zig", zig _, "tree")
  //val treeTest: SearchTree[Int] = Node(Empty,1,Empty)
  val tree1: SearchTree[Int] = Node(Node(Node(Node(Empty,8,Empty),4,Node(Empty,9,Empty)),1,Node(Node(Empty,10,Empty),5,Node(Empty,11,Empty))),2,Node(Node(Node(Empty,12,Empty),6,Node(Empty,13,Empty)),3,Node(Node(Empty,14,Empty),7,Node(Empty,15,Empty))))
  println("Test Tree: " + tree1 + """
      TEST TREE-->   2
                    /   \
                  1       3
                 / \    /   \
                4  5    6     7
              /\  / \  /  \   /\
             8 9 10 11 12 13 14 15
  """)
  println("result zigzag: " + zigzag(tree1))
  println("RESULTS^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^RESULTS")
}
