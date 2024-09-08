package rebecaos.backend

import Semantics.{Act, St, Msg}
import rebecaos.syntax.Show

import caos.sos.SOS

object HistoryState:
  case class HState(s: St, acts: List[Act]):
    override def toString(): String =
      Show(s)

  object HistorySOS:
    def apply(sos:SOS[Act,St]): SOS[Act,HState] = new SOS:
      //type HStatePair = (State,List[Act])
      // specific to our states (not generic)
      def next[A>:Act](s:HState): Set[(A,HState)] =
        for (a,s2) <- sos.next(s.s) yield a -> HState(s2,a :: s.acts)

    /** Build mermaid sequence chart from the SENDERS' perspective.  */
    def toMermaidSnd(hs: HState): String =
//      println(s"acts: ${hs.acts.mkString("\n")}\n")
      "sequenceDiagram\n" +
      (for st <- hs.s._2.toList.sortWith(_._1 < _._1)
        yield s"  ${st._1}  ->> ${st._1}: initial(...)"
//          if hs.acts.exists(act => act._1.rcv==st._1 && act._1.m=="initial")
//          then s"  ${st._1}  ->> ${st._1}: initial(...)"
//          else s"  ${st._1} -->> ${st._1}: initial(...)"
      ).mkString("\n") +
      "\n" +
      (for act <- hs.acts.reverse; m <- act._2.toList // _2 is sent messages
        yield s"  ${getFrom(m)}  ->> ${getTo(m)}: ${getMsg(m)}${getTime(m)}"
//          if //hs.acts.exists(act2 => act2._1.rcv==act._1.rcv && act2._1.m==act._1.m)
//            hs.acts.exists(actRcv => actRcv._1==m)
//          then s"  ${getFrom(m)}  ->> ${getTo(m)}: ${getMsg(m)}${getTime(m)}"
//          else s"  ${getFrom(m)} -->> ${getTo(m)}: ${getMsg(m)}${getTime(m)}"
      ).mkString("\n")
//      (for act <- hs.acts.reverse // _1 is rcv (first to show), _2 is sent messages
//        yield s"  ${getFrom(act._1)}  ->> ${getTo(act._1)}: ${getMsg(act._1)}${getTime(act._1)}"
//      ).mkString("\n") + "\n"+
//      (for act <- hs.acts.reverse; m <- act._2.toList // _1 is rcv (first to show), _2 is sent messages
//        yield s"  ${getFrom(m)}  -->> ${getTo(m)}: ${getMsg(m)}${getTime(m)}"
//      ).mkString("\n")

    /** Build mermaid sequence chart from the RECEIVERS' perspective.  */
    def toMermaidRcv(hs:HState):String = "sequenceDiagram\n" +
      (for act <- hs.acts.reverse yield s"  ${getFrom(act._1)} ->> ${getTo(act._1)}: ${
          getMsg(act._1)}${getTime(act._1)}").mkString("\n")

    /** Build mermaid sequence chart using solid arrows for sent and received messages, and dashed for only received arrows.  */
    def toMermaidSndRcv(hs: HState): String = "sequenceDiagram\n" +
      ((for act <- hs.acts.reverse yield s"  ${getFrom(act._1)} ->> ${getTo(act._1)}: ${
        getMsg(act._1)
      }${getTime(act._1)}")++
      (for m <- hs.s._3.toSet yield s"  ${getFrom(m)} -->> ${getTo(m)}: ${getMsg(m)}${getTime(m)}"))
        .mkString("\n")

    /** Auxiliar functions to build mermaid sequence charts */
    private def getTo(a:Msg): String = a.rcv//"[^.]*".r.findFirstIn(a).getOrElse("X")
    private def getFrom(a:Msg): String = //"@[^ \t\n]*".r.findFirstIn(a).getOrElse("@"+getTo(a)).drop(1)
      if a.snd.isBlank then getTo(a) else a.snd
    private def getMsg(a:Msg): String = s"${a.m}(${a.args.map(Show.apply).mkString(",")})"//"\\.[^@]*".r.findFirstIn(a).getOrElse(".Z").drop(1)
    private def getTime(a:Msg): String = //"§.*".r.findFirstIn(a).getOrElse("§Z").drop(1)
      (a.tt,a.dl) match
        case (0,None) => ""
        case (i,None) => s" @ $i"
        case (i,Some(dl)) => s" @ $i..$dl"
