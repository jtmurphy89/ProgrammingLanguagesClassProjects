// These problems are extracted from "Programming Scala", by
// Dean Wampler.

object Problem1 {
    def main( args:Array[String] ) = {
      // Reverse each element of the args array and print out the
      // result. Note that the String class has a 'reverse' method.
      for(elem <- args){println(elem.reverse)}
    }
}

object Problem2 {
  // A binary tree node.  The field `ord` is declared with
  // `var`, so it is mutable.  For example, you can do:
  //
  // val n = Node(...)
  // n.ord = (1 -> 2)
  //
  // Because we introduced the `var`, you may modify _this_ `var`.
  // You may not introduce any other `var`s.
  case class Node(var ord:(Int,Int), 
                  left:Option[Node],
                  right:Option[Node])

  def main( args:Array[String] ) = {
    // example tree
    val tree = Node( (-1,-1), 
      None,
      Some(Node( (-1,-1),
                Some(Node( (-1,-1), None, None )),
                Some(Node( (-1,-1), Some(Node( (-1,-1), None, None )), None ))
      ))
    )
    
    // set the tree nodes' labels and print the tree. note that case
    // classes are automatically given a toString method, so we don't
    // need to define our own.
    order( tree )
    println( tree )
  }

  def order( node:Node ) {
    def pre(node:Option[Node], accum:Int): Int = {
      node match{
        case None => accum
        case Some(node) => {
          //visit
          node.ord = (accum, node.ord._2)
          //traverse lr
          val retval = pre(node.left, accum+1)
          pre(node.right, retval)
        }
      }
    }
    def post(node:Option[Node], accum:Int): Int = {
      node match{
        case None => accum
        case Some(node) => {
          //traverse
          val retvalL = post(node.left,accum)
          val retvalR = post(node.right, retvalL)
          //visit + increment
          node.ord = (node.ord._1, retvalR)
          retvalR +1 
        }
      }
    }
    pre(Some(node),0)
    post(Some(node),0) 
    // use a nested method inside this method as a helper function to
    // traverse the given tree and set each Node's 'ord' field to the
    // tuple '(preorder, postorder)', where 'preorder' is the Node's
    // preorder label and 'postorder' is the Node's postorder
    // label. For consistent numbers, visit left children before right
    // children. Labels should start at 0 (i.e., the root node's
    // preorder label should be 0).

    // As a hint, you'll need to use recursion here.  The nested
    // method should have an auxilliary parameter, representing the
    // currently available ID.  The nested method should return the
    // next available ID.  This is equivalent to an approach of
    // having a mutable variable elsewhere and incrementing it
    // each time we need a new ID, which is likely a more obvious
    // solution if you're coming from an imperative background.  This
    // is equivalent, because the mutable variable sets up an implicit
    // data dependency between recursive calls, whereas with functional
    // purity we must make this data dependency explicit.
  }
}

object Problem3 {
  def main( args:Array[String] ) = {
    val list = args.toList

    
    // Part 1
    println(list.foldLeft("")((s:String, t:String) => if(s.compare(t) > 0) s else t))
    
    // Part 2
    val r2 = list.foldLeft(List[String]())((l:List[String], s:String) => if(l.contains(s)) l else s::l)
    r2.foreach(println)

    // Part 3
    val r3 = list.foldLeft(List[(String,Int)]())((l: List[(String,Int)], s:String) => if(l.isEmpty || l.head._1 != s) (s,1) :: l else (s,l.head._2 + 1) :: l.tail)
    val runlengthencoded = r3.foldRight("")((t:(String,Int), s:String) => s + t._2.toString + t._1)
    println(runlengthencoded)



    // Use the foldLeft method of list to print the following:
    //
    // 1. the largest element in args (using string comparison)
    // 2. args with no duplicate elements
    // 3. a run-length-encoded version of args

    // NOTES
    //
    // If the initial value given to foldLeft is an empty List you
    // need to explicitly give the type of the List, e.g., List[Int]()
    // or List[String](), otherwise the compiler won't be able to
    // figure out the types.
    //
    // For run-length-encoding specifics, see
    // http://en.wikipedia.org/wiki/Run_length_encoding.
  }
}
