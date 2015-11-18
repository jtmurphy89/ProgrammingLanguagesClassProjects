package miniprologfd.solver

// note the renaming that is being done by these imports
import scala.collection.mutable.{ Map => MMap }
import miniprologfd.interpreter.{ Value, NumValue, SymPlaceholder => Variable }
import miniprologfd.syntax.lowlevel.{ SymbolicROP => ROP, SymEQ => EQ,
  SymNE => NE, SymLT => LT, SymLE => LE, SymGT => GT, SymGE => GE }


// an interval of integers from 0 to Intervals.maxValue, inclusive
case class Interval(low:Int, hi:Int) {
  assert(low >= 0 && hi >= low && hi <= Intervals.maxValue)
  override def toString = "[" + low + ".." + hi + "]"
}


// represents the possible values of a variable as a list of
// intervals. by construction this list contains non-overlapping
// intervals in ascending order.
case class Intervals(ns:List[Interval]) {
  // are there no possible values?
  def isEmpty:Boolean = ns.isEmpty // FILL ME IN

  // apply function f to every possible value
  def foreach( f:Int => Unit ): Unit = ns.foreach(n => {
    f(n.hi) 
    f(n.low)
    })

  // compute the intersection of these possible values and another set
  // of possible values
 def intersect( i:Intervals ): Intervals = Intervals(ns.foldLeft(List[Interval]())((accum:List[Interval],i1:Interval) => {
  i.ns.foldLeft(accum)((accumI:List[Interval], i2:Interval) => {
    (i1,i2) match {
      case (i1,i2) if((i2.low >= i1.low) && (i2.hi <= i1.hi)) => Interval(i2.low, i2.hi)::accumI
      case (i1,i2) if((i2.low <= i1.low) && (i2.hi >= i1.hi)) => Interval(i1.low, i1.hi)::accumI
      case (i1,i2) if((i2.low >= i1.low) && (i1.low > i2.hi) && (i2.hi > i1.hi)) => Interval(i2.low,i1.hi)::accumI
      case (i1,i2) if((i2.low < i1.low) && (i1.low < i2.hi) && (i2.hi <= i1.hi)) => Interval(i1.low,i2.hi)::accumI
      case _ => accumI
    }
  })
}))

  // is there only a single possible value?
  def singleton:Boolean = (ns.size == 1 && ns.head.hi == ns.head.low)

  // retrieve the single possible value (only valid if there is only a
  // single possible value to retrieve)
  def getSingleton:Int = {
    assert(singleton) 
    ns.head.low 
  }
  
  // remove a possible value from the current set of possible values
  def -( n:Int ): Intervals =  intersect(Intervals.getIntervals(miniprologfd.syntax.lowlevel.SymNE, n))

  // return the lowest currently possible value
  def lowest:Int = ns.foldRight(ns.head.low)((i1:Interval, accum:Int) => if(i1.low < accum) i1.low else accum)

  // return the highest currently possible value
  def highest:Int = ns.foldRight(ns.last.hi)((i1:Interval, accum:Int) => if(i1.hi > accum) i1.hi else accum) 

  // returns all currently possible values < n
  def LT( n:Int ): Intervals =  intersect(Intervals.getIntervals(miniprologfd.syntax.lowlevel.SymLT, n))

  // returns all currently possible values ≤ n
  def LE( n:Int ): Intervals =  intersect(Intervals.getIntervals(miniprologfd.syntax.lowlevel.SymLE, n))

  // returns all currently possible values > n
  def GT( n:Int ): Intervals =  intersect(Intervals.getIntervals(miniprologfd.syntax.lowlevel.SymGT, n))

  // returns all currently possible values ≥ n
  def GE( n:Int ): Intervals =  intersect(Intervals.getIntervals(miniprologfd.syntax.lowlevel.SymGE, n))

  override def toString =
    ns.mkString(", ")
}


// companion object with useful functions
object Intervals {
  // maximum possible value
  val maxValue = 500

  // default Intervals value, i.e., the default set of possible values
  // for any variable
  val default = Intervals(List(Interval(0, maxValue)))

  // create an Intervals with a single possible value
  def apply( n:Int ): Intervals = Intervals(List[Interval](Interval(n,n)))

  // create an Intervals with a set of contiguous possible values in
  // the range low to hi
  def apply( low:Int, hi:Int ): Intervals = Intervals(List[Interval](Interval(low, hi)))

  // Create an Intervals containing all possible values that have the
  // given relation op with the given integer n.  `max` below refers
  // to the `maxValue` constant defined above.
  //
  // examples: getIntervals(<,  10) = [[0..9]]
  //           getIntervals(=,  10) = [[10..10]]
  //           getIntervals(≠,  10) = [[0..9], [11..max]]
  //           getIntervals(<,   0) = []          // less than minimum
  //           getIntervals(>, max) = []          // greater than maximum
  //           getIntervals(=,  -1) = []          // out of range
  //           getIntervals(>,  -5) = [[0..max]]  // the whole valid range
  def getIntervals( op:ROP, n:Int ): Intervals = {
    (op,n) match{
      case (EQ, n) if(n >= 0 && n <= maxValue) => Intervals(n)
      case (NE,n) if(n > 0 && n < maxValue) => Intervals(List[Interval](Interval(0,n-1), Interval(n+1,maxValue)))
      case (NE, n) if(n < 0 || n > maxValue) => Intervals(0,maxValue)
      case (NE, n) if(n == 0) => Intervals(1,maxValue)
      case (NE, n) if(n == maxValue) => Intervals(0,maxValue-1)
      case (LT,n) if(n > 0 && n <= maxValue) => Intervals(0,n-1)
      case (LT, n) if(n > maxValue) => Intervals(0,maxValue)
      case (LE, n) if(n >= 0 && n <= maxValue) => Intervals(0,n)
      case (LE, n) if(n > maxValue) => Intervals(0,maxValue)
      case (GT, n) if(n >= 0 && n < maxValue) => Intervals(n+1, maxValue)
      case (GT, n) if(n < 0) => Intervals(0,maxValue)
      case (GE, n) if(n >= 0 && n <= maxValue) => Intervals(n,maxValue)
      case (GE, n) if(n < 0) => Intervals(0,maxValue)
      case _ => Intervals(List[Interval]())
  }
 }
}


// constraints between variables
case class Constraint(op:ROP, x1:Variable, x2:Variable)


// a map from variables to their possible values
case class Values( vs:Map[Variable, Intervals] = Map() ) {
  // retrieve the possible values for the given variable; if the
  // variable isn't in vs then return the default set of possible
  // values
  def apply( x:Variable ): Intervals = if(vs.get(x) != None) vs(x) else Intervals(0, Intervals.maxValue)

  // return a mutable copy of this map
  def mutableCopy:MValues = MValues(MMap() ++ vs)
}


// a mutable map from variables to their possible values
case class MValues( vs:MMap[Variable, Intervals] = MMap() ) {
  // retrieve the possible values for the given variable; if the
  // variable isn't in vs then return the default set of possible
  // values
  def apply( x:Variable ): Intervals = if(vs.get(x) != None) vs(x) else Intervals(0, Intervals.maxValue)

  // update the possible values of the given variable
  def update( x:Variable, i:Intervals ) { 
    vs.update(x,i)
  }

  // clear the map
  def clear() {
    vs.clear
  }

  // is the map empty?
  def isEmpty:Boolean = vs.isEmpty

  // return an immutable copy of this map
  def immutableCopy:Values = Values(Map() ++ vs)
}


// a map from variables to the constraints mentioning that variable
case class Constraints( cs:Map[Variable, Set[Constraint]] = Map() ) {
  // retrieve the constraint set for the given variable; if the
  // variable isn't in cs then return the empty set
  def apply( x:Variable ): Set[Constraint] = if(cs.get(x) != None) cs(x) else Set()


  // add constraint c to the constraints sets for variables x1 and x2
  // mentioned in constraint c
  def +( c:Constraint ): Constraints = Constraints(cs + (c.x1 -> (apply(c.x1) + c)) + (c.x2 -> (apply(c.x2) + c)))
}


// this is the actual solver, including an API used by the
// miniprolog-fd interpreter to insert constraints and query for
// satisfiable values.
case class ConstraintStore(
  values:Values = Values(),
  constraints:Constraints = Constraints()
) {

  ////////// API FOR INTERPRETER //////////

  // insert a new constraint into the constraint store. the return
  // value is None if the new constraint is inconsistent with existing
  // constraints, otherwise it is Some(the updated constraint store).
   def insert(op: ROP, v1: Value, v2: Value): Option[ConstraintStore] = (v1,v2) match {
    case (NumValue(n), v:Variable) => {
      val possibleValues =  Intervals.getIntervals(reverseOp(op), n)
      var mutableCV = values.mutableCopy
      propagate(mutableCV, v, possibleValues)
      if(!mutableCV.isEmpty) Some(ConstraintStore(mutableCV.immutableCopy,constraints)) else None
    }
    case (v:Variable, NumValue(n)) => {
      val possibleValues =  Intervals.getIntervals(op, n)
      var mutableCV = values.mutableCopy
      propagate(mutableCV, v, possibleValues)
      if(!mutableCV.isEmpty) Some(ConstraintStore(mutableCV.immutableCopy,constraints)) else None
    }
    case (x1:Variable, x2:Variable) => {
      if((values(x1).singleton) && (values(x2).singleton)) {
        if(compare(values(x1).getSingleton,op,values(x2).getSingleton)) Some(this)
        else None
      }
      else {
        val cStore = ConstraintStore(values, constraints + Constraint(op,x1,x2)).solve(values, List(x1,x2))
        if(cStore == None) None
        else Some(ConstraintStore(values, constraints + Constraint(op,x1,x2)))
      }
    }
    case _ => None
  }

  def compare(n1: Int, op: ROP, n2: Int): Boolean = {
    op match {
      case LT => (n1 < n2)
      case LE => (n1 <= n2)
      case GT => (n1 > n2)
      case GE => (n1 >= n2)
      case EQ => (n1 == n2)
      case NE => (n1 != n2)
    }
  }

  // query the constraint store for satisfying values for the given
  // list of variables
 def SAT(xs: List[Variable]): List[NumValue] = solve(values,xs) match {
    case Some(vs) => xs.map(v => NumValue(vs(v).getSingleton))
    case None => List[NumValue]()
  }


  ////////// INTERNAL METHODS //////////


  // given the current possible values for all the variables, compute
  // satisfying values for the given list of variables
  def solve( currValues:Values, xs:List[Variable] ): Option[Values] = {
    val x = xs.head
    val t1 = xs.tail
    for(y <- currValues(x)) {
      var mutableCV = currValues.mutableCopy
      propagate(mutableCV, x, Intervals(List(Interval(y,y))))
      if(!mutableCV.isEmpty){
        val currValuesN = mutableCV.immutableCopy
        if(!t1.isEmpty){
          solve(currValuesN, t1) match {
            case None => 
            case Some(cv) => return Some(cv) 
           }  
          }
        else return Some(currValuesN)
        }
      }
      return None
    }
  // constraint propagation: restrict the possible values of x to only
  // include values in ranges. if that makes x unsatisfiable then
  // return; if that changes the possible values of x then recursively
  // propagate those changes to the other variables.
  //
  // IMPORTANT NOTE: because mutableCV returns a default set of
  // possible values for any variable not contained in the mapping, we
  // can't just iterate through all the constraints as done in the
  // pseudocode (it would incorrectly propagate default values if
  // mutableCV is made empty in some recursive call to
  // propagate). instead, we need to explicitly check after each
  // recursive call to propagate whether mutableCV is empty, and if so
  // then explicitly return from the function.
  def propagate( mutableCV:MValues, x:Variable, ranges:Intervals) {
    val check = mutableCV.immutableCopy(x)
    mutableCV(x) = mutableCV(x).intersect(ranges)
    if(mutableCV(x).isEmpty){
      mutableCV.clear
      return
    }
    else if(check != mutableCV.immutableCopy(x)){
      constraints(x).foreach { c => 
        val x2 = if(x == c.x1) {c.x2} else c.x1
        val r2 = narrow(x2, mutableCV(x2), c, mutableCV(x))
        propagate(mutableCV, x2, r2)
        if(mutableCV.isEmpty) {
          mutableCV.clear
          return
        }
      }
    }
  }

  // narrow the current possible values of x1 (given as rangesX1)
  // according to the constraint c, which is between x1 and some
  // variable x2 (where rangesX2 gives the possible values of x2)
  def narrow( x1:Variable, rangesX1:Intervals, c:Constraint, rangesX2:Intervals ): Intervals = {
    c match{
        case Constraint(EQ, `x1`,_) | Constraint(EQ, _,`x1`) => rangesX1.intersect(rangesX2)
        case Constraint(NE, `x1`,_) | Constraint(NE, _,`x1`) => if(rangesX2.singleton) rangesX1 - rangesX2.getSingleton else rangesX1
        case Constraint(LT, `x1`,_) | Constraint(GT, _,`x1`) => rangesX1.LT(rangesX2.highest)
        case Constraint(LE, `x1`,_) | Constraint(GE, _,`x1`) => rangesX1.LE(rangesX2.highest)
        case Constraint(GT, `x1`,_) | Constraint(LT, _,`x1`) => rangesX1.GT(rangesX2.lowest)
        case Constraint(GE, `x1`,_) | Constraint(LE, _,`x1`) => rangesX1.GE(rangesX2.lowest)
    }
  }

  // reverse the meaning of a relational operator (e.g., < becomes >
  // and ≤ becomes ≥)
  def reverseOp( op:ROP ): ROP =
    op match {
      case EQ => EQ
      case NE => NE
      case LT => GT
      case LE => GE
      case GT => LT
      case GE => LE
    }
}
