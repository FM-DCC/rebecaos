package rebecaos.backend

import caos.sos.SOS
import rebecaos.backend.Semantics.St
import rebecaos.syntax.Program.{BExpr, Expr, GVar, IExpr, InstanceDecl, Msgsrv, QVar, ReactiveClass, Statement, System}
import Statement.*
import IExpr.*
import BExpr.*
import rebecaos.syntax.Show

import scala.annotation.targetName

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[String,St]:

  type St = (System, Rebecs, Msgs)

  type Rebecs = Map[String,RebecEnv]
  case class RebecEnv(vars:Valuation, rebs:KnownReb, meth:Methods, clazz: String):
    @targetName("addValuation")
    def ++(vars2:Valuation) = RebecEnv(vars++vars2, rebs, meth, clazz)
    @targetName("addAssignment")
    def +(vd:(String,Boolean|Int)) = RebecEnv(vars+vd,rebs,meth,clazz)
    @targetName("addRebec")
    def +(kn:(String,String)) = RebecEnv(vars,rebs+kn,meth,clazz)
    def now:Int = vars.getOrElse("now",0) match
      case i:Int => i
      case b => sys.error(s"variable 'now' should be an int, but it is '$b'.")
  type Data = Boolean|Int
  type Valuation = Map[String,Data]
  type KnownReb  = Map[String,String] // from local names to system names
  type Methods   = Map[String,Msgsrv]

  type Msgs = Bag[Msg] // bag of Class/Instance name, Method name, Arguments
  case class Msg(rcv:String,m:String,args:List[Data],snd:String,tt:Int,dl:Option[Int])
               // receiver, method, args, sender, timeSend, deadline

  def initSt(s: System): St =
    (s, getInstances(s).toMap, Bag(getInitMsg(s)) )

  def getInstances(s: System): List[(String,RebecEnv)] =
    for dec <- s.main yield
      val vars = Map("now"->0)
      val clazz = s.classes.getOrElse(dec.clazz, sys.error(s"Unknown rebeca class: ${dec.clazz}"))
      val meth = clazz.msgsrv
      val rebs = unifyReb(clazz.known, dec.known) + ("self"->dec.name) //no sender
      dec.name -> RebecEnv(vars,rebs,meth,dec.clazz)

  def getInitMsg(s: System): List[Msg] =
    for dec <- s.main yield
      //println(s"added ${dec.name}.initial(${dec.args})")
      Msg(dec.name,"initial",dec.args,"",0,None)

//  /** Given a rebeca class name, a method name, a list of arguments, and a program, returns a concrete rebeca instance. */
//  def instantiate(dec: InstanceDecl, msgsrvN:String, s:System): RebecInst =
//    val clazz = s.classes.getOrElse(dec.clazz, sys.error(s"Unknown rebeca class: ${dec.clazz}"))
//    val msgsrv = clazz.msgsrv.getOrElse(msgsrvN, sys.error(s"Unknown msgsrv ${dec.clazz}.$msgsrvN"))
//    val env = unify(msgsrv.vars,dec.args)
//    val knownReb = unifyReb(clazz.known,dec.known) + ("self"->dec.name)
//    val stm = substReb(msgsrv.stm)(using knownReb)
//    //    val knownReb = if clazz.known.size != known
//    //      then sys.error(s"Wrong number of known: found [${known.mkString(",")}], expected ${clazz.known.size} ")
//    //      else clazz.known.zip(known).toMap
//    //    val stm = evalCalls(msgsrv.stm clazz.known
//    //    val x = subst(msgsrv.stm)(using msgsrv.vars.map(_.name).zip(decl.args).toMap) // ingoring types of arguments
//    RebecInst(stm,env,Bag(),dec.clazz) //
//
  def unify(vars: List[QVar], vals: List[Int|Boolean]): Valuation = (vars,vals) match
    case (Nil,Nil) => Map()
    case (QVar(v,"int")::restr, (d:Int)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,"byte")::restr, (d:Int)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,"bool")::restr, (d:Boolean)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,t)::restr, d::restl) => sys.error(s"Value $d: ${d.getClass.toString} does not match variable $v: $t.")
    case (Nil,_) => sys.error(s"Unexpected actual arguments: ${vals.mkString(",")}")
    case (_,Nil) => sys.error(s"Unexpected formal arguments: ${vars.mkString(",")}")

  def unifyReb(vars: List[QVar], vals: List[String]): Map[String,String] = (vars,vals) match
    case (Nil,Nil) => Map()
    case (QVar(v,typ)::rest1, arg::rest2) => unifyReb(rest1,rest2)+(v->arg)
    case (Nil, _) => sys.error(s"Unexpected actual rebecs: ${vals.mkString(",")}")
    case (_, Nil) => sys.error(s"Unexpected formal rebecs: ${vars.mkString(",")}")

  def next[A>:String](st: St): Set[(A, St)] =
//    val someInitial = st._3.bag.find((m,_)=>m.m=="initial")
    val initials = for m <- st._3.bag.keySet if m.m=="initial" yield m.rcv
    for
//      msg@Msg(rcv,m,args,snd,tt,dl) <- st._3.toSet if someInitial.isEmpty || someInitial.get._1==msg // for each message
      msg@Msg(rcv,m,args,snd,tt,dl) <- st._3.toSet if (!initials(rcv)) || (initials(rcv) && m=="initial") // priority to initials
      rebEnv <- st._2.get(rcv).toSet  // getting the potential receiver
      mth <- rebEnv.meth.get(m).toSet // getting the potential method
      (newEnv,newMsgs) <- evalStm( mth.stm)(using rebEnv ++ // evaluate the satement with extra variables:
          unify(mth.vars,args) +         // adding vars=values
          ("now" -> (rebEnv.now max tt)) + // setting "now" value
          ("sender" -> snd) // setting the "sender"
        )
    yield
      val updMsg = newMsgs.map(m => subst(m,rebEnv.rebs + ("self"->rcv)))
      //println(s"# msg $rcv.$m - old msgs: ${st._3}; dropped $msg; adding ${newMsgs} --> $updMsg")
      s"${Show(msg)}"
        -> (st._1, st._2 + (rcv -> newEnv), (st._3 - msg) ++ updMsg)

  def evalStmDb(stm:Statement)(using reb: RebecEnv): Set[(RebecEnv, Msgs)] =
    println(s"evaluating $stm knowing $reb")
    val res = evalStm(stm)
    println(s"--> Got: $res")
    res

  def evalStm(stm:Statement)(using reb: RebecEnv): Set[(RebecEnv, Msgs)] = stm match
    case Skip => Set(reb -> Bag())
    case Seq(Skip, c2) => evalStm(c2)
    case Seq(c1, c2) =>
      for
        (sigma,msgs) <- evalStm(c1)
        (sigma2,msgs2) <- evalStm(c2)(using reb++sigma.vars)
      yield
        sigma2 -> (msgs ++ msgs2)
    case Assign(ident, e) =>
      val reb2 = reb + (ident -> eval(e)(using reb))
      Set(reb2 -> Bag())
    case ITE(b, ct, cf) => if (eval(b)(using reb))
      then evalStm(ct)
      else evalStm(cf)
    case Choice(v, options) =>
      for opt <- options.toSet yield
        val newopt = eval(opt)(using reb)
        val sigma2 = reb + (v -> newopt)
        sigma2 -> Bag()
    case Call(rebec, meth, args, after, deadline) =>
      val after2 = after.map(e=> eval(e)(using reb))
      val deadline2 = deadline.map(e=> eval(e)(using reb))
      val tt = reb.now + after2.getOrElse(0)
      val dl = deadline2.map(_ + reb.now)
//      println(s"DEADLINE - updated from $deadline to $deadline2 to $dl (and 'after' set to $after2 to $tt)")
//      println(s"msg: ${Show(Msg(rebec,meth,args.map(e=>eval(e)(using reb.vars)),"self",tt,dl))} -- ${Msg(rebec,meth,args.map(e=>eval(e)(using reb.vars)),"self",tt,dl)}")
      Set(reb -> (Bag() + Msg(rebec,meth,args.map(e=>eval(e)(using reb)),"self",tt,dl)))
//    case Call(rebec, meth, args, _, _) => sys.error("Semantics of calls with time not yet supported.")
    case Delay(d) => // sys.error("Semantics of delay not yet supported.")
      val d2 = eval(d)(using reb)
      val tt = reb.vars.get("now") match
        case Some(i: Int) => i + d2
        case None => d2
        case Some(b) => sys.error(s"variable 'now' should be an int, but it is '$b'.")
      Set(reb+("now"->tt) -> Bag())

  def evalReb(rebName:String, rebecs: Rebecs): RebecEnv =
    rebecs.getOrElse(rebName, sys.error(s"Rebec instance '$rebName' not found - known: {${rebecs.keySet.mkString(",")}}"))



  /////////////////////
  // Auxiliar: replacing values and evaluating integers/booleans
  /////////////////////

  def subst(m:Msg,knownReb: KnownReb): Msg =
    def upd(s:String) = knownReb.getOrElse(s,s)
    Msg(upd(m.rcv),m.m,m.args,upd(m.snd),m.tt,m.dl)

//  def subst(stm:Statement)(using env:Valuation): Statement = stm match
//    case Skip => Skip
//    case Seq(c1, c2) => Seq(subst(c1),subst(c2))
//    case Assign(ident, e) => Assign(ident,subst(e))
//    case ITE(b, ct, cf) => ITE(subst(b),subst(ct),subst(cf))
//    case Choice(v, options) => Choice(v,options.map(subst))
//    case Call(rebec, meth, args, after, dl) => Call(rebec, meth, args.map(subst), after.map(subst), dl.map(subst))
//    case Delay(d) => Delay(subst(d))
//
//
//  def subst(e: Expr)(using env: Valuation): Expr = e match
//    case GVar(ident) => env.get(ident) match
//      case Some(value: Int) => N(value)
//      case Some(true) => BTrue
//      case Some(false) => BFalse
//      case None => e
//    case ie: IExpr => subst(ie)
//    case be: BExpr => subst(be)
//
//  def subst(e: IExpr)(using env: Valuation): IExpr = e match
//    case N(n) => e
//    case IVar(ident) => env.get(ident) match
//      case Some(value: Int) => N(value)
//      case Some(value: Boolean) => sys.error(s"Variable $ident has type Boolean ($value), but expected an Int.")
//      case None => e
//    case Plus(e1, e2) => Plus(subst(e1), subst(e2))
//    case Times(e1, e2) => Times(subst(e1), subst(e2))
//    case Minus(e1, e2) => Minus(subst(e1), subst(e2))
//    case Power(e1, e2) => Power(subst(e1), subst(e2))
//  def subst(b:BExpr)(using env:Valuation): BExpr = b match
//    case BVar(name) => env.get(name) match
//      case Some(true) => BTrue
//      case Some(false) => BFalse
//      case Some(value: Int) => sys.error(s"Variable $name has type Int ($value), but expected a Boolean.")
//      case None => b
//    case And(b1, b2) => And(subst(b1), subst(b2))
//    case Or(b1, b2) => Or(subst(b1), subst(b2))
//    case Not(b1) => Not(subst(b1))
//    case Less(e1, e2) => Less(subst(e1), subst(e2))
//    case Greater(e1, e2) => Greater(subst(e1), subst(e2))
//    case Eq(e1, e2) => Eq(subst(e1), subst(e2))
//    case Impl(e1, e2) => Impl(subst(e1), subst(e2))
//    case _ => b

  def substReb(stm: Statement)(using rebs:Map[String,String]): Statement = stm match
    case Skip => Skip
    case Seq(c1, c2) => Seq(substReb(c1),substReb(c2))
    case Assign(ident, e) => stm
    case ITE(b, ct, cf) => ITE(b,substReb(ct),substReb(cf))
    case Choice(v, options) => stm
    case Call(rebec, meth, args, after, dl) =>
      Call(rebs.getOrElse(rebec,sys.error(s"rebec $rebec not found in $stm")),meth,args,after,dl)
    case Delay(d) => stm

  def eval(e: Expr)(using env: RebecEnv): Int|Boolean = e match
    case GVar("false") => false
    case GVar("true") => true
    case GVar(name) => env.vars.get(name) match
      case Some(value) => value
      case None => env.rebs.get(name) match
        case Some(str) => str.hashCode
        case None => sys.error(s"Unknown variable: '$name'")
//    case IVar("false") => false
//    case IVar("true") => true
    case iexpr: IExpr => eval(iexpr)
    case bexpr: BExpr => eval(bexpr)

  def eval(e: IExpr)(using env: RebecEnv): Int = e match
    case N(n) => n
    case IVar(ident) => env.vars.get(ident) match
      case Some(value: Int) => value
      case Some(value: Boolean) => sys.error(s"Variable $ident has type Boolean ($value), but expected an Int.")
      case None => env.rebs.get(ident) match
        case Some(str) => str.hashCode
        case None => sys.error(s"Unknown integer variable: '$ident'")
    case Plus(e1, e2) => eval(e1) + eval(e2)
    case Times(e1, e2) => eval(e1) * eval(e2)
    case Minus(e1, e2) => eval(e1) - eval(e2)
    case Power(e1, e2) => Math.pow(eval(e1), eval(e2)).toInt

  def eval(e: BExpr)(using env: RebecEnv): Boolean = e match
    case BTrue => true
    case BFalse => false
    case BVar(ident) => env.vars.get(ident) match
      case Some(value: Boolean) => value
      case Some(value: Int) => sys.error(s"Variable $ident has type Int ($value), but expected an Boolean.")
      case None => sys.error(s"Unknown boolean variable: '$ident'")
    case And(b1, b2) => eval(b1) && eval(b2)
    case Or(b1, b2) => eval(b1) || eval(b2)
    case Not(b) => !eval(b)
    case Less(e1, e2) => toInt(eval(e1)) < toInt(eval(e2))
    case Greater(e1, e2) => toInt(eval(e1)) > toInt(eval(e2))
    case Eq(e1, e2) => eval(e1) == eval(e2)
    case Impl(e1, e2) => (!eval(e1)) || eval(e2)

  private def toInt(x:Int|Boolean): Int = x match
    case i:Int => i
    case _ => sys.error(s"Expected int but found'$x'")
