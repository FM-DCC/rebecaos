package rebecaos.backend

import caos.sos.SOS
import rebecaos.backend.Semantics.{Act, Rebecs, St}
import rebecaos.syntax.Program.{Expr, InstanceDecl, Msgsrv, QVar, ReactiveClass, Statement, System}
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

  /* Initial state of a system S */
  def initSt(s: System): St =
    RebecEnv.restart
    (s, getInstances(s).toMap, Bag(getInitMsg(s)) )

  /* Collect all instance declarations in the `main` block. */
  def getInstances(s: System): List[(String,RebecEnv)] =
    for dec <- s.main yield
      instantiate(dec, s, Data.N(0))

  /** Create a pair with the variable name and a fresh state */
  def instantiate(dec: InstanceDecl, s:System, now:Data): (String,RebecEnv) =
    val clazz = s.classes.getOrElse(dec.clazz, sys.error(s"Unknown rebeca class: ${dec.clazz}"))
    val meth = clazz.msgsrv
    val rebs = unifyReb(clazz.known, dec.known) + ("self"->dec.name) //no sender
    dec.name -> (RebecEnv(Map("now"->now),meth,dec.clazz) ++ rebs)

  /* Build a message `initial` for all instances declared in the `main` block. */
  def getInitMsg(s: System): List[Msg] =
    for dec <- s.main yield
      val x = Msg(dec.name,"initial",dec.args.map(a=>Eval(a)(using RebecEnv.empty)),"",0,None)
      x

  /* Try to unify qualified variables. */
  def unify(vars: List[QVar], vals: List[Data]): Valuation = (vars,vals) match
    case (Nil,Nil) => Map()
    case (QVar(v,"int")::restr, (d:Data)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,"byte")::restr, (d:Data)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,"boolean")::restr, (d:Data)::restl) => unify(restr,restl)+(v->d)
    case (QVar(v,t)::restr, d::restl) => sys.error(s"Value $d: ${d.getClass.toString} does not match variable $v: $t.")
    case (Nil,_) => sys.error(s"Unexpected actual arguments: ${vals.mkString(",")}")
    case (_,Nil) => sys.error(s"Unexpected formal arguments: ${vars.mkString(",")}")

  /* Try to unify variables with rebecs. */
  def unifyReb(vars: List[QVar], vals: List[String]): Map[String,String] = (vars,vals) match
    case (Nil,Nil) => Map()
    case (QVar(v,typ)::rest1, arg::rest2) => unifyReb(rest1,rest2)+(v->arg)
    case (Nil, _) => sys.error(s"Unexpected actual rebecs: ${vals.mkString(",")}")
    case (_, Nil) => sys.error(s"Unexpected formal rebecs: ${vars.mkString(",")}")

  /* Calculate the set of possible next steps of a given state.
     Each step includes an action (message received) and a new state. */
  def next[A>:Act](st: St): Set[(A, St)] =
    val initials = for m <- st._3.bag.keySet if m.m=="initial" yield m.rcv
    for
      smallestTT <- st._3.bag.keySet.map(_.tt).minOption.toSet
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
      (msg,updMsg)
        -> (st._1, (st._2 + (rcv -> newEnv)) ++ newRebs, (st._3 - msg) ++ updMsg)

  /* Checks if a given message is enabled, given priority to `initial` states. */
  def enabled(m: Msg, initials: Set[String],smallestTT: Int): Boolean =
    ((!initials(m.rcv)) || (initials(m.rcv) && m.m=="initial")) &&
      m.tt <= smallestTT

  /* Checks if a given message is enabled based on its deadline. */
  def enabledDL(m: Msg, env: RebecEnv): Boolean =
      m.dl.isEmpty || (m.dl.get >= env.now)

  /* Evaluates a (non-deterministic) statement, returning a set of possible state updates. */
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
      val rebID = RebecEnv.newVar
      val localName = dec.name // localName = new ...
      val clazz = syst.classes.getOrElse(dec.clazz, sys.error(s"Unknown rebeca class: ${dec.clazz}")) // class definition used for the new rebec
      val meth = clazz.msgsrv // needed to the state (env) of the new rebec
      val knwonRebs = unifyReb(clazz.known, dec.known.map(reb.getReb))
      val newRebState = RebecEnv(Map("now"->Data.N(reb.now),"self"->Data.RebRef(rebID)),meth,dec.clazz)++knwonRebs
      val msg = Msg(rebID,"initial",dec.args.map(Eval(_)),reb.getReb("self"),reb.now,None)
      Set((reb + (localName->rebID), Bag()+msg, Map(rebID -> newRebState)))
    case Call(rebVar, meth, args, after, deadline) =>
      val r = reb.getReb(rebVar)
      val after2 = after.map(Eval(_))
      val deadline2 = deadline.map(Eval(_))
      val tt = reb.now + after2.map(_.toInt).getOrElse(0)
      val dl = deadline2.map(_.toInt + reb.now)
      Set((reb,
           Bag() + Msg(r,meth,args.map(Eval(_)),"self",tt,dl),
           Map()))
    case Delay(d) =>
      val tt = reb.now + Eval(d).toInt
      Set((reb+("now"->Data.N(tt)) , Bag() , Map()))

  /* Tries to retrieve a specific rebec, throwing an error message in case of failure. */
  def evalReb(rebName:String, rebecs: Rebecs): RebecEnv =
    rebecs.getOrElse(rebName, sys.error(s"Rebec instance '$rebName' not found - known: {${rebecs.keySet.mkString(",")}}"))



  /////////////////////
  // Auxiliar: replacing values and checking requirements
  /////////////////////

  /* Replaces message names based on a given mapping of names. */
  def subst(m:Msg,updMap:Map[String,String]): Msg =
    def upd(s:String) = updMap.getOrElse(s,s)
    Msg(upd(m.rcv),m.m,m.args,upd(m.snd),m.tt,m.dl)


  /* Traverses the state space using random-walks while checking if a set of queries are reached. */
  def checkReqs(s:St, max:Int=5000): (Map[Expr,(String,String)],Int,Boolean) =
    val totalReq = s._1.reqs.size
    def aux(nextSt:Map[St,List[Act]], done:Set[St],
            edges:Int, limit:Int,
            reqReached: Map[Expr,(String,String)]): (Map[Expr,(String,String)],Int,Boolean) =
      if limit <=0 then
        return (reqReached,edges,false)
      if reqReached.size >= totalReq then
        return (reqReached,edges,true)
      nextSt.headOption match
        case None =>
          (reqReached, edges, true)
        case Some((st,_)) if done contains st =>
          aux(nextSt-st,done,edges,limit,reqReached)
        case Some((st,trace)) => //visiting new state
          val more = next(st)
          val checks = check(st,trace,reqReached,more.isEmpty)
          aux((nextSt-st)++more.map(as=>(as._2 -> (as._1::trace))).toMap, done+st, edges+more.size,limit-more.size,checks)

    def check(st:St, tr:List[Act], reqs:Map[Expr,(String,String)],dead:Boolean): Map[Expr,(String,String)] =
      val newReq = for req <- st._1.reqs
                       if !reqs.contains(req)  &&
                         (checkExpr(req,st._2) ||
                           (req==Expr.Var("deadlock") && dead))
        yield req -> (tr.reverse.map(x=>Show(x._1)).mkString(" > ") -> Show(st))
      reqs ++ newReq.toMap

    def checkExpr(exp:Expr,rebs:Rebecs): Boolean =
      try
        Eval.eval(exp)(using rebs) != Data.B(false)
      catch
        case Eval.UnkonwnElm(_) => false
        case t:Throwable => throw t

    aux(Map(s->Nil), Set(), 0, max, Map())
