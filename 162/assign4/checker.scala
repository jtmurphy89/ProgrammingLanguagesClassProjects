import scala.io._
import cs162.assign4.syntax._
import Aliases._
import scala.io.Source.fromFile

//——————————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        // try {
          println(Pretty.prettySyntax(program))
          getType( program.e, new TypeEnv())
          println("This program is well-typed")
          ///println(Pretty.prettySyntax(program))
       // } catch { case Illtyped => println("failed.") }
    }
  }

  // Gets a listing of the constructor names associated with a given type definition.
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructors`, along with return values:
  //
  // constructors("Either") = Set("Left", "Right")
  // constructors("Foo") = a thrown Illtyped exception
  //
  def constructors(name: Label): Set[Label] =
    typeDefs.find(_.name == name).map(_.constructors.keySet).getOrElse(throw Illtyped)

  // Takes the following parameters:
  // -The name of a user-defined type
  // -The name of a user-defined constructor in that user-defined type
  // -The types which we wish to apply to the constructor
  // Returns the type that is held within the constructor.
  //
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructorType`, along with return values:
  //
  // constructorType("Either", "Left", Seq(NumT)) = NumT
  // constructorType("Either", "Right", Seq(BoolT)) = BoolT
  // constructorType("Either", "Left", Seq(NumT, NumT)) = a thrown Illtyped exception
  // constructorType("Either", "Foo", Seq(UnitT)) = a thrown Illtyped exception
  // constructorType("Bar", "Left", Seq(UnitT)) = a thrown Illtyped exception
  //
  def constructorType(name: Label, constructor: Label, types: Seq[Type]): Type = 
    (for {
      td <- typeDefs.find(_.name == name)
      rawType <- td.constructors.get(constructor)
      if (types.size == td.tvars.size)
    } yield replace(rawType, td.tvars.zip(types).toMap)).getOrElse(throw Illtyped)

  // Given a type and a mapping of type variables to other types, it
  // will recursively replace the type variables in `t` with the
  // types in `tv2t`, if possible.  If a type variable isn't
  // in `tv2t`, it should simply return the original type.  If a
  // `TFunT` is encountered, then whatever type variables it defines
  // (the first parameter in the `TFunT`) should overwrite whatever is in
  // `tv2t` right before a recursive `replace` call.  In other words,
  // type variables can shadow other type variables.
  //
  def replace( t:Type, tv2t:Map[TVar, Type] ): Type =
    t match {
      case NumT | BoolT | UnitT => t

      case FunT(params, ret) => FunT(params.map((typo:Type) => replace(typo,tv2t)), replace(ret, tv2t))

      case RcdT(fields) => RcdT(fields.mapValues(replace(_, tv2t)))

      case TypT(name, typs) => TypT(name, typs.map((typo:Type) => replace(typo,tv2t)))

      case tv:TVar => tv2t.getOrElse(tv, t)

      case TFunT(tvars, funt) => TFunT(tvars,replace(funt, tv2t -- tvars).asInstanceOf[FunT])
    }

  // HINT - the bulk of this remains unchanged from the previous assignment.
  // Feel free to copy and paste code from your last submission into here.
  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      case x:Var => env.getOrElse(x,throw Illtyped) // FILL ME IN

      case _:Num => NumT // FILL ME IN

      case _:Bool => BoolT // FILL ME IN

      case _:Unit => UnitT // FILL ME IN

      case Plus | Minus | Times | Divide => FunT(Seq(NumT,NumT), NumT) // FILL ME IN

      case LT | EQ => FunT(Seq(NumT,NumT), BoolT) // FILL ME IN

      case And | Or => FunT(Seq(BoolT,BoolT), BoolT) // FILL ME IN

      case Not => FunT(Seq(BoolT), BoolT) // FILL ME IN

      case Fun(params, body) => {
         val t2 = getType(body, params.foldLeft(env)((accum:TypeEnv, t:(Var,Type)) => accum + (t._1 -> t._2)))
         FunT(params.foldLeft(Seq[Type]())((accum:Seq[Type], t:(Var,Type)) => accum:+t._2), t2)
       }

      case Call(fun, args) => getType(fun, env) match {
        case FunT(params, body) => {
            val e2ty = args.foldRight(Seq[Type]())((e:Exp, accum:Seq[Type]) => getType(e, env)+:accum)
            if (e2ty == params) body
            else throw Illtyped
        }
        case _ => throw Illtyped
      } 

      case If(e1, e2, e3) => getType(e1, env) match {
        case BoolT => {
          val a = getType(e2, env)
          val b = getType(e3, env)
          if(a == b) a 
          else throw Illtyped
        }
        case _ => throw Illtyped
      } 

      case Let(x, e1, e2) => {
        val e1Type = getType(e1,env)
        val e2Type = getType(e2, env + (x -> e1Type))
        e2Type
      } 

      case Rec(x, t1, e1, e2) => {
        val e1Type = getType(e1, env + (x -> t1))
        val e2Type = getType(e2, env + (x -> t1))
        if(e1Type == t1) e2Type else throw Illtyped
      }
      

      case Record(fields) => {
        val field = fields.map((t:(Label,Exp)) => (t._1 -> getType(t._2,env)))
        RcdT(field)
      }

      case Access(e, field) => getType(e, env) match {
        case RcdT(fields) => {
          fields.getOrElse(field, throw Illtyped)
        }
        case _ => throw Illtyped
      }

      case c @ Construct(name, constructor, typs, e) => {
        val t1 = constructorType(name, constructor, typs)
        val eType = getType(e, env)
        if(t1 == eType) TypT(name, typs) 
        else throw Illtyped
      }

      case Match(e, cases) => getType(e,env) match {
        case TypT(nombre, typo) => {
          val t0 = cases.map((t:(Label,Var,Exp)) => t._1)
          val t1 = cases.map((t:(Label,Var,Exp)) => getType(t._3, env + (t._2 -> constructorType(nombre, t._1, typo))))
          if(t0.distinct.sorted != constructors(nombre).toSeq.sorted || t0.distinct.length != t0.length ) throw Illtyped
          else if(t1.distinct.length == 1) t1.head
          else throw Illtyped
        }
        case _ => throw Illtyped
      }

      case TAbs(tvars, fun) => getType(fun,env) match {
        case f:FunT => {
          TFunT(tvars, f)
        }
        case _ => throw Illtyped
      }

      case TApp(e, typs) => getType(e, env) match {
        case TFunT(tvars,funnyBitch) => {
          if(tvars.length == typs.length) replace(funnyBitch, tvars.zip(typs).toMap)
          else throw Illtyped
        }
        case _ => throw Illtyped
      } // FILL ME IN
    }
}
