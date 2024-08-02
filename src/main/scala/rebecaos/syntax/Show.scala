package rebecaos.syntax

import Program.{Expr2, InstanceDecl, Msgsrv, ReactiveClass, Statement, System}
import rebecaos.backend.Eval
//import rebecaos.backend.Semantics.{RebecInst, Rebecs, St}
import rebecaos.backend.{Semantics,RebecEnv}
import rebecaos.backend.Semantics.Msg

/**
 * List of functions to produce textual representations of commands
 */
object Show:

  def apply(st: Semantics.St): String =
//    s"## System ##\n${apply(st._1)}\n
    s"## Rebecs ##\n${applyR(st._2)}\n## Messages ##\n${st._3.map(apply)}"

  def applyR(rbs: Semantics.Rebecs): String =
    rbs.map((k, v) => s"$k => ${apply(v)}").mkString("\n")

  def apply(rb: RebecEnv): String =
    s"[${rb.vars.map((v, d) => s"$v:$d").mkString(",")}]{${rb.rebs.map((v, d) => s"$v:$d").mkString(",")}}:${rb.clazz}"

  def apply(msg: Msg): String =
    s"${msg.rcv}.${msg.m}(${msg.args.map(apply).mkString(",")})${
      if msg.snd=="" then "" else s"@${msg.snd}"}${
      if msg.tt>0 then s" after ${msg.tt}" else ""}${
      if msg.dl.nonEmpty then s" deadline ${msg.dl.get}" else ""
    }"

  def short(st: Semantics.St): String =
    st._2.map((n,e) => s"$n${if e.now>0 then s"-${e.now}" else ""}").mkString(" ")


  def apply(s:System): String =
    s.classes.map(showClass).mkString("\n")+
      "\nmain:"+
      s.main.map(showInstDecl).mkString
  def showClass(cl:(String,ReactiveClass)): String =
    s"reactiveclass ${cl._1} (${cl._2.qsize.map(_.toString).getOrElse("")}):${
      cl._2.known.map(x => s"\n  known ${x.name}: ${x.typ}").mkString}${
      cl._2.state.map(x => s"\n  var ${x.name}: ${x.typ}").mkString}${
      cl._2.msgsrv.map(x => s"\n  msgsrv ${x._1}:${showMsgSrv(x._2)}").mkString}"
  def showInstDecl(i:InstanceDecl): String =
    s"\n  ${i.clazz} ${i.name}(${i.known.mkString(",")}):(${i.args.map(apply).mkString(",")})"
  def showMsgSrv(ms: Msgsrv): String =
    s"${ms.vars.map(x=>x.name+": "+x.typ).mkString(", ")}${
      "\n    "+showStms(ms.stm).mkString("\n    ")
    }"

  def apply(e:Expr2): String = e match
    case Expr2.N(n) => n.toString
    case Expr2.B(b) => b.toString
    case Expr2.RebRef(r) => r
    case Expr2.Var(v) => v
    case Expr2.Infix(op, e1, e2) => s"${exprPar(e1)} $op ${exprPar(e2)}"
    case Expr2.Func("!", List(e)) => s"!${exprPar(e)}"
    case Expr2.Func(op,es) => s"$op(${es.map(apply).mkString(",")})"
  private def exprPar(e:Expr2): String = e match
    case i:Expr2.Infix => s"(${apply(e)})"
    case _ => apply(e)

  def apply(d: Eval.Data): String = d match
    case Eval.Data.N(n) => n.toString
    case Eval.Data.B(b) => b.toString
    case Eval.Data.RebRef(r) => s"&$r"


  def showStms(s: Statement): List[String] = s match
    case Statement.Seq(e1, e2) => showStms(e1) ::: showStms(e2)
    case Statement.ITE(b,ct,cf) =>
      s"if ${apply(b)}:"::(
        showStms(ct).map(x=>s"  $x"):::(
          if cf==Statement.Skip then Nil else
      "else:"::
        showStms(cf).map(x=>s"  $x")))
    case Statement.Assign(ident,e) =>
      List(s"$ident := ${apply(e)}")
    case Statement.Skip => Nil
    case Statement.Call(rebec, meth, args, after, dl) =>
      List(s"${rebec}.${meth}(${args.map(apply).mkString(",")})${
        if after.nonEmpty then s" after ${apply(after.get)}" else ""}${
        if dl.nonEmpty then s" deadline ${apply(dl.get)}" else ""
      }")
    case Statement.NewReb(dec) =>
      List(showInstDecl(dec).drop(3))
    case Statement.Choice(ident,es) =>
      showStms(Statement.Assign(ident,Expr2.Func("?",es)))
    case Statement.Delay(e) =>
      List(s"delay ${apply(e)}")


//  def apply(st: St): String =
//    s"## System ##\n${apply(st._1)}\n## State ##\n${apply(st._2)}"
//
//  def apply(sys: System): String =
//    sys.classes.map((n, rb) => s"$n -> $rb").mkString("\n") // {apply(rb)}
//
//  def apply(rbs: Rebecs): String =
//    rbs.map((k, v) => s"$k => ${apply(v)}").mkString("\n")
//
//  def apply(rb: RebecInst): String =
//    s"{${rb.stm}}[${rb.sigma.map((v, d) => s"$v:$d").mkString(",")}](${rb.msgs})"

//
//
//  /** Converts the main term into a mermaid diagram reflecting its structure. */
//  def mermaid(s:System): String = "graph TD\n" +
//  s"  style ${s.main.hashCode()} fill:#ffe177,stroke:#6a6722,stroke-width:4px;\n" +
//    (term2merm(s.defs) ++
//     term2merm(s.main)).mkString("\n")
//
//      /** Builds nodes and arcs, using a set structure to avoid repetition. */
//  private def term2merm(e: Term): Set[String] = e match
//    case Term.End => Set(s"  ${e.hashCode()}([\"0\"])")
//    case Term.Proc(p) => Set(s"  ${e.hashCode()}([\"$p\"])")
//    case Term.Prefix(act, t) => term2merm(t) ++
//      Set(s"  ${e.hashCode()} -->|action| ${act.hashCode()}",
//          s"  ${e.hashCode()} -->|rest| ${t.hashCode()}",
//          s"  ${e.hashCode()}([\"${apply(e)}\"])",
//          s"  ${act.hashCode()}([\"$act\"])"
//        )
//    case Term.Choice(t1, t2) =>
//      term2merm(t1) ++ term2merm(t2) ++
//      Set(s"  ${e.hashCode()} -->|option 1| ${t1.hashCode()}",
//          s"  ${e.hashCode()} -->|option 2| ${t2.hashCode()}",
//          s"  ${e.hashCode()}([\"${apply(e)}\"])")
//    case Term.Par(t1, t2) =>
//      term2merm(t1) ++ term2merm(t2) ++
//      Set(s"  ${e.hashCode()} -->|par 1| ${t1.hashCode()}",
//          s"  ${e.hashCode()} -->|par 2| ${t2.hashCode()}",
//          s"  ${e.hashCode()}([\"${apply(e)}\"])")
//
//  /** Builds the nodes and arcs of the diagram of the definitions */
//  private def term2merm(defs: Map[String,Term]): Set[String] =
//    defs.flatMap( (p,t) =>
//      term2merm(t) +
//      s"  ${Proc(p).hashCode()}([\"$p\"])" +
//      s"  ${Proc(p).hashCode()} -->|definition| ${t.hashCode}"
//    ).toSet
