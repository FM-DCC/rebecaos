package rebecaos.backend

import caos.sos.SOS
import rebecaos.backend.Semantics.{Act, Rebecs, St}
import rebecaos.syntax.Program.{Expr, GVar, InstanceDecl, Msgsrv, QVar, ReactiveClass, Statement, System}
import RebecEnv.*
import Statement.*
import rebecaos.backend.Eval.Data
import rebecaos.syntax.Show

import scala.annotation.targetName

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[Act,St]:

  type St = (System, Rebecs, Msgs)
  type Act = (Msg,Msgs) // "received" and "sent"

  type Rebecs = Map[String,RebecEnv]

  type Msgs = Bag[Msg] // bag of Class/Instance name, Method name, Arguments
  case class Msg(rcv:String,m:String,args:List[Data],snd:String,tt:Int,dl:Option[Int])
               // receiver, method, args, sender, timeSend, deadline

  def initSt(s: System): St =
    RebecEnv.restart
    (s, getInstances(s).toMap, Bag(getInitMsg(s)) )

  def getInstances(s: System): List[(String,RebecEnv)] =
    for dec <- s.main yield
      instantiate(dec, s, Data.N(0))

  /** Creates a pair with the variable name and a fresh state */
  def instantiate(dec: InstanceDecl, s:System, now:Data): (String,RebecEnv) =
    val clazz = s.classes.getOrElse(dec.clazz, sys.error(s"Unknown rebeca class: ${dec.clazz}"))
    val meth = clazz.msgsrv
    val rebs = unifyReb(clazz.known, dec.known) + ("self"->dec.name) //no sender
    dec.name -> (RebecEnv(Map("now"->now),meth,dec.clazz) ++ rebs)

  def getInitMsg(s: System): List[Msg] =
    for dec <- s.main yield
      //println(s"added ${dec.name}.initial(${dec.args.map(a=>Eval(a)(using RebecEnv.empty))})")
      val x = Msg(dec.name,"initial",dec.args.map(a=>Eval(a)(using RebecEnv.empty)),"",0,None)
      x

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
  def unify(vars: List[QVar], vals: List[Data]): Valuation = (vars,vals) match
    case (Nil,Nil) => Map()
    case (QVar(v,"int")::restr, (d:Data)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,"byte")::restr, (d:Data)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,"boolean")::restr, (d:Data)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,t)::restr, d::restl) => sys.error(s"Value $d: ${d.getClass.toString} does not match variable $v: $t.")
    case (Nil,_) => sys.error(s"Unexpected actual arguments: ${vals.mkString(",")}")
    case (_,Nil) => sys.error(s"Unexpected formal arguments: ${vars.mkString(",")}")

  def unifyReb(vars: List[QVar], vals: List[String]): Map[String,String] = (vars,vals) match
    case (Nil,Nil) => Map()
    case (QVar(v,typ)::rest1, arg::rest2) => unifyReb(rest1,rest2)+(v->arg)
    case (Nil, _) => sys.error(s"Unexpected actual rebecs: ${vals.mkString(",")}")
    case (_, Nil) => sys.error(s"Unexpected formal rebecs: ${vars.mkString(",")}")

  def next[A>:Act](st: St): Set[(A, St)] =
//    val someInitial = st._3.bag.find((m,_)=>m.m=="initial")
    val initials = for m <- st._3.bag.keySet if m.m=="initial" yield m.rcv
    for
      smallestTT <- st._3.bag.keySet.map(_.tt).minOption.toSet
//      msg@Msg(rcv,m,args,snd,tt,dl) <- st._3.toSet if someInitial.isEmpty || someInitial.get._1==msg // for each message
      msg@Msg(rcv,m,args,snd,tt,dl) <- st._3.toSet if enabled(msg,initials,smallestTT) // priority to initials
      rebEnv <- st._2.get(rcv).toSet if enabledDL(msg,rebEnv) // getting the potential receiver
      mth <- rebEnv.meth.get(m).toSet // getting the potential method
      (newEnv,newMsgs,newRebs) <- evalStm( mth.stm)(using rebEnv ++ // evaluate the satement with extra variables:
          unify(mth.vars,args) +         // adding vars=values
          ("now" -> Data.N(rebEnv.now max tt)) + // setting "now" value
          ("sender" -> Data.RebRef(snd)), // setting the "sender"
          st._1
        )
    yield
      val updMsg = newMsgs.map(m => subst(m,Map("self"->rcv)))
//      println(s"# msg $rcv.$m - old msgs: ${st._3}; dropped $msg; adding ${newMsgs} --> $updMsg")
      (msg,updMsg)
        -> (st._1, (st._2 + (rcv -> newEnv)) ++ newRebs, (st._3 - msg) ++ updMsg)

  def enabled(m: Msg, initials: Set[String],smallestTT: Int): Boolean =
    ((!initials(m.rcv)) || (initials(m.rcv) && m.m=="initial")) &&
      m.tt <= smallestTT

  def enabledDL(m: Msg, env: RebecEnv): Boolean =
      m.dl.isEmpty || (m.dl.get >= env.now)

  def evalStmDb(stm:Statement)(using reb: RebecEnv, sys:System): Set[(RebecEnv, Msgs, Rebecs)] =
    println(s"evaluating $stm knowing $reb")
    val res = evalStm(stm)
    println(s"--> Got: $res")
    res

  def evalStm(stm:Statement)(using reb: RebecEnv, syst:System): Set[(RebecEnv, Msgs, Rebecs)] = stm match
    case Skip => Set((reb,Bag(),Map()))
    case Seq(Skip, c2) => evalStm(c2)
    case Seq(c1, c2) =>
      for
        (sigma,msgs,r1) <- evalStm(c1)
        (sigma2,msgs2,r2) <- evalStm(c2)(using reb++sigma.vars)
      yield
        (sigma2 , msgs ++ msgs2 , r1 ++ r2)
    case Assign(ident, e) =>
      val reb2 = reb + (ident -> Eval(e))
      //println(s"[:=] added ${ident}<-${Eval(e)}")
      Set((reb2 , Bag() , Map()))
    case ITE(b, ct, cf) => if (Eval(b)(using reb).toBool)
      then evalStm(ct)
      else evalStm(cf)
    case Choice(v, options) =>
      for opt <- options.toSet yield
        val newopt = Eval(opt)(using reb)
        val sigma2 = reb + (v -> newopt)
        (sigma2 , Bag() , Map())
    case NewReb(dec) =>
      // need to:
      // (1) create a new instance with a global name,
      //     replacing only known rebecs and using a correct "now", and
      // (2) add a new msg to initialise it, evaluating arguments
      val rebID = RebecEnv.newVar
      val localName = dec.name // localName = new ...
      val clazz = syst.classes.getOrElse(dec.clazz, sys.error(s"Unknown rebeca class: ${dec.clazz}")) // class definition used for the new rebec
      val meth = clazz.msgsrv // needed to the state (env) of the new rebec
      val knwonRebs = unifyReb(clazz.known, dec.known.map(reb.getReb))
      val newRebState = RebecEnv(Map("now"->Data.N(reb.now),"self"->Data.RebRef(rebID)),meth,dec.clazz)++knwonRebs
      val msg = Msg(rebID,"initial",dec.args.map(Eval(_)),reb.getReb("self"),reb.now,None)
//      println(s"[:=new] added ${localName}<-${rebID}")
      Set((reb + (localName->rebID), Bag()+msg, Map(rebID -> newRebState)))
    case Call(rebVar, meth, args, after, deadline) =>
      val r = reb.getReb(rebVar)
      val after2 = after.map(Eval(_))
      val deadline2 = deadline.map(Eval(_))
      val tt = reb.now + after2.map(_.toInt).getOrElse(0)
      val dl = deadline2.map(_.toInt + reb.now)
//      println(s"DEADLINE - updated from $deadline to $deadline2 to $dl (and 'after' set to $after2 to $tt)")
//      println(s"msg: ${Show(Msg(rebec,meth,args.map(e=>eval(e)(using reb.vars)),"self",tt,dl))} -- ${Msg(rebec,meth,args.map(e=>eval(e)(using reb.vars)),"self",tt,dl)}")
      Set((reb,
           Bag() + Msg(r,meth,args.map(Eval(_)),"self",tt,dl),
           Map()))
//    case Call(rebec, meth, args, _, _) => sys.error("Semantics of calls with time not yet supported.")
    case Delay(d) => // sys.error("Semantics of delay not yet supported.")
      val tt = reb.now + Eval(d).toInt
//        reb.vars.get("now") match
//        case Some(i: Int) => i + d2
//        case None => d2
//        case Some(b) => sys.error(s"variable 'now' should be an int, but it is '$b'.")
      Set((reb+("now"->Data.N(tt)) , Bag() , Map()))

  def evalReb(rebName:String, rebecs: Rebecs): RebecEnv =
    rebecs.getOrElse(rebName, sys.error(s"Rebec instance '$rebName' not found - known: {${rebecs.keySet.mkString(",")}}"))



  /////////////////////
  // Auxiliar: replacing values and evaluating integers/booleans
  /////////////////////

  def subst(m:Msg,updMap:Map[String,String]): Msg =
    def upd(s:String) = updMap.getOrElse(s,s)
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

//  def substReb(stm: Statement)(using rebs:Map[String,String]): Statement = stm match
//    case Skip => Skip
//    case Seq(c1, c2) => Seq(substReb(c1),substReb(c2))
//    case Assign(ident, e) => stm
//    case ITE(b, ct, cf) => ITE(b,substReb(ct),substReb(cf))
//    case Choice(v, options) => stm
//    case Call(rebec, meth, args, after, dl) =>
//      Call(rebs.getOrElse(rebec,sys.error(s"rebec $rebec not found in $stm")),meth,args,after,dl)
//    case Delay(d) => stm

//  def eval(e: Expr)(using env: RebecEnv): Int|Boolean = e match
//    case GVar("false") => false
//    case GVar("true") => true
//    case GVar(name) => env.vars.get(name) match
//      case Some(value) => value
//      case None => env.rebs.get(name) match
//        case Some(str) => str.hashCode
//        case None => sys.error(s"Unknown variable: '$name'")
////    case IVar("false") => false
////    case IVar("true") => true
//    case iexpr: IExpr => eval(iexpr)
//    case bexpr: BExpr => eval(bexpr)
//
//  def eval(e: IExpr)(using env: RebecEnv): Int = e match
//    case N(n) => n
//    case IVar(ident) => env.vars.get(ident) match
//      case Some(value: Int) => value
//      case Some(value: Boolean) => sys.error(s"Variable $ident has type Boolean ($value), but expected an Int.")
//      case None => env.rebs.get(ident) match
//        case Some(str) => str.hashCode
//        case None => sys.error(s"Unknown integer variable: '$ident'")
//    case Plus(e1, e2) => eval(e1) + eval(e2)
//    case Times(e1, e2) => eval(e1) * eval(e2)
//    case Minus(e1, e2) => eval(e1) - eval(e2)
//    case Power(e1, e2) => Math.pow(eval(e1), eval(e2)).toInt
//
//  def eval(e: BExpr)(using env: RebecEnv): Boolean = e match
//    case BTrue => true
//    case BFalse => false
//    case BVar(ident) => env.vars.get(ident) match
//      case Some(value: Boolean) => value
//      case Some(value: Int) => sys.error(s"Variable $ident has type Int ($value), but expected an Boolean.")
//      case None => sys.error(s"Unknown boolean variable: '$ident'")
//    case And(b1, b2) => eval(b1) && eval(b2)
//    case Or(b1, b2) => eval(b1) || eval(b2)
//    case Not(b) => !eval(b)
//    case Less(e1, e2) => toInt(eval(e1)) < toInt(eval(e2))
//    case Greater(e1, e2) => toInt(eval(e1)) > toInt(eval(e2))
//    case Eq(e1, e2) => eval(e1) == eval(e2)
//    case Impl(e1, e2) => (!eval(e1)) || eval(e2)
//
//  private def toInt(x:Int|Boolean): Int = x match
//    case i:Int => i
//    case _ => sys.error(s"Expected int but found'$x'")
