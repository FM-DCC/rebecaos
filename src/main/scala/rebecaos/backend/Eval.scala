package rebecaos.backend

import rebecaos.backend.RebecEnv
import rebecaos.syntax.Program.Expr

object Eval:

  enum Data:
    case N(n:Int)
    case B(b:Boolean)
    case RebRef(r:String)

    def toInt: Int = this match
      case N(n) => n
      case _ => sys.error(s"Expected 'int'; found '$this'.")
    def toBool: Boolean = this match
      case B(b) => b
      case _ => sys.error(s"Expected 'boolean'; found '$this'.")
    def toReb: String = this match
      case RebRef(r) => r
      case _ => sys.error(s"Expected 'RebRef'; found '$this'.")

  def apply(e:Expr)(using env: RebecEnv) : Data = e match
    case Expr.N(n) => Data.N(n)
    case Expr.B(b) => Data.B(b)
    //case Expr2.RebRef(r) => Data.RebRef(r) // ignoring call to rebec reference
    case Expr.Var(v) =>  env.vars.get(v) match
      case Some(Data.N(n)) => Data.N(n)
      case Some(Data.B(n)) => Data.B(n)
      case Some(Data.RebRef(r)) => Data.RebRef(r)
//      case None => env.rebs.get(v) match
//        case Some(str) => Data.RebRef(str)
      case None => sys.error(s"Unknown variable '$v'.")
//    case Expr2.NewReb(_) => Data.RebRef(RebecEnv.newVar) // ignoring call
    case Expr.Infix("&&",e1,e2) => Data.B(apply(e1).toBool && apply(e2).toBool)
    case Expr.Infix("||",e1,e2) => Data.B(apply(e1).toBool || apply(e2).toBool)
    case Expr.Infix("=>",e1,e2) => Data.B((!apply(e1).toBool) ||  apply(e2).toBool)
    case Expr.Func("not",List(e1)) => Data.B(!(apply(e1).toBool))
    case Expr.Infix("+",e1,e2) => Data.N(apply(e1).toInt + apply(e2).toInt)
    case Expr.Infix("-",e1,e2) => Data.N(apply(e1).toInt - apply(e2).toInt)
    case Expr.Infix("*",e1,e2) => Data.N(apply(e1).toInt * apply(e2).toInt)
    case Expr.Infix("/",e1,e2) => Data.N(apply(e1).toInt / apply(e2).toInt)
    case Expr.Infix("%",e1,e2) => Data.N(apply(e1).toInt % apply(e2).toInt)
    case Expr.Infix("^",e1,e2) => Data.N(math.pow(apply(e1).toInt,apply(e2).toInt).toInt)
    case Expr.Infix("<",e1,e2) => Data.B(apply(e1).toInt < apply(e2).toInt)
    case Expr.Infix(">",e1,e2) => Data.B(apply(e1).toInt > apply(e2).toInt)
    case Expr.Infix("<=",e1,e2) => Data.B(apply(e1).toInt <= apply(e2).toInt)
    case Expr.Infix(">=",e1,e2) => Data.B(apply(e1).toInt >= apply(e2).toInt)
    case Expr.Infix("==",e1,e2) => Data.B(apply(e1) == apply(e2))
    case Expr.Infix("!=",e1,e2) => Data.B(apply(e1) != apply(e2))

    case Expr.Infix(op,_,_) => sys.error(s"Unknonw operator '$op' in $e")
    case Expr.Func(op,_) => sys.error(s"Unknonw function '$op' in $e")

