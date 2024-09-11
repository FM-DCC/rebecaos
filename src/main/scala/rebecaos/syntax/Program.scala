package rebecaos.syntax

import rebecaos.backend.Eval
/**
 * Internal structure to represent terms in rebecaOS.
 */

object Program:

  /** Full system: collection of reactive classes' definitions, followed by the concrete instances */
  case class System(classes: Map[String,ReactiveClass], main: List[InstanceDecl])

  /** Declaration of an instance of a rebec (actor) */
  case class InstanceDecl(clazz: String, name: String, known: List[String], args: List[Expr])

  /** Reactive class */
  case class ReactiveClass(qsize:Option[Int], known: List[QVar], state: List[QVar], msgsrv: Map[String,Msgsrv])

  /** Qualified variable */
  case class QVar(name: String, typ: String)

  /** Message server (method of an actor) */
  case class Msgsrv(vars: List[QVar],  stm: Statement)

  /** Statement (or sequence of) */
  enum Statement:
    case Skip
    case Seq(c1: Statement, c2: Statement)
    case Assign(ident: String, e: Expr)
    case ITE(b: Expr, ct: Statement, cf: Statement)
    case Choice(v: String, options: List[Expr])
    case NewReb(dec: InstanceDecl)
    case Call(rebec: String, meth: String, args: List[Expr], after: Option[Expr], dl: Option[Expr])
    case Delay(d: Expr)

  enum Expr:
    case N(n:Int)
    case B(b:Boolean)
    case Var(v:String)
    case Infix(op:String, e1:Expr, e2:Expr)
    case Func(op:String,es:List[Expr])
//    case RebRef(r:String)
//    case NewReb(c:Statement.Call)

  object Expr:

    def imply(b1: Expr, b2: Expr): Expr = (b1, b2) match //Or(Not(b1),b2)
      case (Expr.B(true), _) => b2
      case (_, Expr.B(false)) => not(b1)
      case (Expr.B(false), _) | (_, Expr.B(true)) => Expr.B(true)
      case _ => Expr.Infix("=>",b1, b2)

    def and(b1: Expr, b2: Expr): Expr = (b1, b2) match
      case (Expr.B(true), _) => b2
      case (_, Expr.B(true)) => b1
      case (Expr.B(false), _) | (_, Expr.B(false)) => Expr.B(false)
      case _ => Infix("&&", b1, b2)

    def or(b1: Expr, b2: Expr): Expr = (b1, b2) match
      case (Expr.B(false), _) => b2
      case (_, Expr.B(false)) => b1
      case (Expr.B(true), _) | (_, Expr.B(true)) => Expr.B(true)
      case _ => Infix("||", b1, b2)

    def not(b1: Expr): Expr = b1 match
      case Expr.B(true) => Expr.B(false)
      case Expr.B(false) => Expr.B(true)
      case _ => Func("not", List(b1))


  //////////DEPRECATED?

  trait ExprDepr
  case class GVar(name:String) extends ExprDepr
  object ExprDepr:
    def toExpr(x:Int|Boolean): ExprDepr = x match
      case i:Int => IExpr.N(i)
      case true => BExpr.BTrue
      case false => BExpr.BFalse

  /** An integer expression */
  enum IExpr extends ExprDepr:
    case N(n: Int)
    case IVar(ident: String)
    case Plus(e1: IExpr, e2: IExpr)
    case Times(e1: IExpr, e2: IExpr)
    case Minus(e1: IExpr, e2: IExpr)
    case Power(e1: IExpr, e2: IExpr)

  /** A boolean expression in the while language */
  enum BExpr extends ExprDepr:
    case BTrue
    case BFalse
    case BVar(name:String)
    case And(b1: BExpr, b2: BExpr)
    case Or(b1: BExpr, b2: BExpr)
    case Not(b: BExpr)
    case Less(e1: ExprDepr, e2: ExprDepr)
    case Greater(e1: ExprDepr, e2: ExprDepr)
    case Eq(e1: ExprDepr, e2: ExprDepr)
    case Impl(e1: BExpr, e2: BExpr)

  object BExpr:
    def imply(b1: BExpr, b2: BExpr): BExpr = (b1, b2) match //Or(Not(b1),b2)
      case (BTrue, _) => b2
      case (_, BFalse) => not(b1)
      case (BFalse, _) | (_, BTrue) => BTrue
      case _ => Impl(b1, b2)

    def and(b1: BExpr, b2: BExpr): BExpr = (b1, b2) match
      case (BTrue, _) => b2
      case (_, BTrue) => b1
      case (BFalse, _) | (_, BFalse) => BFalse
      case _ => And(b1, b2)

    def or(b1: BExpr, b2: BExpr): BExpr = (b1, b2) match
      case (BFalse, _) => b2
      case (_, BFalse) => b1
      case (BTrue, _) | (_, BTrue) => BTrue
      case _ => Or(b1, b2)

    def not(b1: BExpr): BExpr = b1 match
      case BTrue => BFalse
      case BFalse => BTrue
      case _ => Not(b1)



  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////

//  object Examples:
//    import Program.Term._
//
//
//    val p1: Term =
//      Prefix("x",End)

