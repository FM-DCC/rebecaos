package rebecaos.syntax

import rebecaos.backend.Eval
/**
 * Internal structure to represent terms in rebecaOS.
 */

object Program:

  /** Full system: collection of reactive classes' definitions, followed by the concrete instances */
  case class System(classes: Map[String,ReactiveClass], main: List[InstanceDecl])

  /** Declaration of an instance of a rebec (actor) */
  case class InstanceDecl(clazz: String, name: String, known: List[String], args: List[Expr2])

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
    case Assign(ident: String, e: Expr2)
    case ITE(b: Expr2, ct: Statement, cf: Statement)
    case Choice(v: String, options: List[Expr2])
    case NewReb(dec: InstanceDecl)
    case Call(rebec: String, meth: String, args: List[Expr2], after: Option[Expr2], dl: Option[Expr2])
    case Delay(d: Expr2)
//    case While(b: BExpr, c: Command, inv: BExpr)
//    case Assert(b: BExpr)
//    case Fail
//    case Contract(pre: BExpr, c: Command, pos: BExpr)



  enum Expr2:
    case N(n:Int)
    case B(b:Boolean)
    case RebRef(r:String)
    case Var(v:String)
//    case NewReb(c:Statement.Call)
    case Infix(op:String,e1:Expr2,e2:Expr2)
    case Func(op:String,es:List[Expr2])

  object Expr2:

    def imply(b1: Expr2, b2: Expr2): Expr2 = (b1, b2) match //Or(Not(b1),b2)
      case (Expr2.B(true), _) => b2
      case (_, Expr2.B(false)) => not(b1)
      case (Expr2.B(false), _) | (_, Expr2.B(true)) => Expr2.B(true)
      case _ => Expr2.Infix("=>",b1, b2)

    def and(b1: Expr2, b2: Expr2): Expr2 = (b1, b2) match
      case (Expr2.B(true), _) => b2
      case (_, Expr2.B(true)) => b1
      case (Expr2.B(false), _) | (_, Expr2.B(false)) => Expr2.B(false)
      case _ => Infix("&&", b1, b2)

    def or(b1: Expr2, b2: Expr2): Expr2 = (b1, b2) match
      case (Expr2.B(false), _) => b2
      case (_, Expr2.B(false)) => b1
      case (Expr2.B(true), _) | (_, Expr2.B(true)) => Expr2.B(true)
      case _ => Infix("||", b1, b2)

    def not(b1: Expr2): Expr2 = b1 match
      case Expr2.B(true) => Expr2.B(false)
      case Expr2.B(false) => Expr2.B(true)
      case _ => Func("not", List(b1))


  //////////DEPRECATED?

  trait Expr
  case class GVar(name:String) extends Expr
  object Expr:
    def toExpr(x:Int|Boolean): Expr = x match
      case i:Int => IExpr.N(i)
      case true => BExpr.BTrue
      case false => BExpr.BFalse

  /** An integer expression */
  enum IExpr extends Expr:
    case N(n: Int)
    case IVar(ident: String)
    case Plus(e1: IExpr, e2: IExpr)
    case Times(e1: IExpr, e2: IExpr)
    case Minus(e1: IExpr, e2: IExpr)
    case Power(e1: IExpr, e2: IExpr)

  /** A boolean expression in the while language */
  enum BExpr extends Expr:
    case BTrue
    case BFalse
    case BVar(name:String)
    case And(b1: BExpr, b2: BExpr)
    case Or(b1: BExpr, b2: BExpr)
    case Not(b: BExpr)
    case Less(e1: Expr, e2: Expr)
    case Greater(e1: Expr, e2: Expr)
    case Eq(e1: Expr, e2: Expr)
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

