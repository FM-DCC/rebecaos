package rebecaos.syntax

import Program.System
//import rebecaos.backend.Semantics.{RebecInst, Rebecs, St}
import rebecaos.backend.Semantics
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

  def apply(rb: Semantics.RebecEnv): String =
    s"[${rb.vars.map((v, d) => s"$v:$d").mkString(",")}]{${rb.rebs.map((v, d) => s"$v:$d").mkString(",")}}:${rb.clazz}"

  def apply(msg: Msg): String =
    s"${msg.rcv}.${msg.m}(${msg.args.mkString(",")})${
      if msg.snd=="" then "" else s"@${msg.snd}"}${
      if msg.tt>0 then s" after ${msg.tt}" else ""}${
      if msg.dl.nonEmpty then s" deadline ${msg.dl.get}" else ""
    }"

  def short(st: Semantics.St): String =
    st._2.map((n,e) => s"$n${if e.now>0 then s"-${e.now}" else ""}").mkString(" ")


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
