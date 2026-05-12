package rebecaos.syntax

import rebecaos.backend.Eval
/**
 * Internal structure to represent terms in rebecaOS.
 */

object Program:

  /** Full system: collection of reactive classes' definitions, followed by the concrete instances */
  case class System(classes: Map[String,ReactiveClass], main: List[InstanceDecl], reqs: List[Expr]=Nil)

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
    case Var(v:String,prefix:String="")
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
