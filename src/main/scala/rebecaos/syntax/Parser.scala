package rebecaos.syntax

import cats.parse.Parser.*
import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Numbers.*
import rebecaos.backend.Eval.Data
import rebecaos.syntax.Program.*
import rebecaos.syntax.Program.IExpr.*
import rebecaos.syntax.Program.BExpr.*
import rebecaos.syntax.Program.Statement.*

import scala.sys.error

object Parser :

  /** Parse a command  */
  def parseProgram(str:String):System =
    pp(system,str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }

  /** Applies a parser to a string, and prettifies the error message */
  def pp[A](parser:P[A], str:String): Either[String,A] =
    parser.parseAll(str) match
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  /** Prettifies an error message */
  private def prettyError(str:String, err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${("-" * (y+1))+"^\n"}""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"

  // Simple parsers for spaces and comments
  /** Parser for a sequence of spaces or comments */
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
//    (string("/*") *> P.charsWhile0())
  private val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  val commentRest: P[Unit] = P.recursive(rest =>
      P.charsWhile0(_!='*').with1 *> char('*') *>//"[^*]\\*".r
      ((char('/') | rest))
    )
  val commentBlk: P[Unit] = string("/*") *> commentRest //P.charsWhile0()
  private val sps: P0[Unit] = (whitespace | comment | commentBlk).rep0.void
  private val fsps: P[Unit] = (whitespace | comment | commentBlk).rep.void

  // Parsing smaller tokens
  private def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  private def anyName: P[String] =
    alphaDigit.rep.string
  private def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  private def className: P[String] =
    (charIn('A' to 'Z') ~ alphaDigit.rep0).string
  private def symbols: P[String] =
    // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as symbols of terms
    P.not(string("--")).with1 *>
    oneOf("+-><!%/*=|&".toList.map(char)).rep.string
  private def intVal: P[Int] =
    digits.map(_.toInt)

  import scala.language.postfixOps

  /** A program is a command with possible spaces or comments around. */
//  private def program: P[System] =
//    char(' ').as(System(Map(),Term.End,None))
//    ((oneProgram<*sps)~((char('~')*>sps*>(term<*sps))?))
//      .map((x,y) => System(x.defs,x.main,y))
//  private def oneProgram: P[System] =
//    system|term.map(x=>System(Map(),x,None))
//  private def system: P[System] =
//    string("let") *> sps *>
//    ((defn.repSep0(sps)<*sps<*string("in")<*sps).with1 ~ term)
//      .map((x,y)=>System(x.toMap,y,None))
//  private def defn:P[(String,Term)] =
//    (procName <* char('=').surroundedBy(sps)) ~
//      (term <* sps <* char(';'))
//
//  private def term: P[Term] = P.recursive(more =>
//    termSum(more).repSep(sps *> char('|') <* sps)
//      .map(l => l.toList.tail.foldLeft(l.head)((t1, t2) => Par(t1, t2)))
//  )
//  private def termSum(more:P[Term]): P[Term] =
//    (termSeq(more)<*sps).repSep(char('+') <* sps)
//      .map(l=>l.toList.tail.foldLeft(l.head)((t1,t2)=>Choice(t1,t2)))
//
//  private def termSeq(more:P[Term]): P[Term] = P.recursive(t2 =>
//    end | proc | pref(t2) | char('(')*>more.surroundedBy(sps)<*char(')')
//  )
//
//  private def end: P[Term] =
//    char('0').as(End)
//
//  private def proc: P[Term] =
//    procName.map(Proc.apply)
//
//  private def pref(t2:P[Term]): P[Term] =
//    ((varName <* sps) ~ ((char('.') *> t2)?))
//      .map(x => Prefix(x._1,x._2.getOrElse(End)))


  /////////// NEW for Rebeca

  def system: P[System] =
    (sps.with1 *>
      reactiveclass.repSep0(sps).with1 ~
      (sps.with1 *> mainblock) <*
      sps
    ).map(x => System(x._1.toMap,x._2))


  def mainblock: P[List[InstanceDecl]] =
    string("main") *> sps *>
      ((char('{') *> sps *> instancedecl.repSep0(sps)) <* sps <* char('}'))
        .map(_.toList)

  def instancedecl: P[InstanceDecl] =
    //className rebecName(⟨rebecName⟩∗) : (⟨literal⟩∗);
    ((className <* sps) ~ // classname
      (varName <* sps) ~ // rebecName
      ((char('(') *> sps *> varName.repSep0(sps ~ char(',') ~ sps)) <* sps <* char(')')) ~ // known rebecs
      ((sps *> char(':') *> sps *> char('(') *>
        //(digits.map(s => Data.N(s.toInt)) | string("true").as(Data.B(true)) | string("false").as(Data.B(false)))
        expr2
          .repSep0(sps ~ char(',') ~ sps)) <*
        sps <* char(')') <* ending ) // queue sizes
    ).map(x => InstanceDecl(x._1._1._1,x._1._1._2,x._1._2,x._2))


  def reactiveclass: P[(String,ReactiveClass)] =
    ((string("reactiveclass") *> sps *>
      (className <* sps)) ~ // classname
      (char('(') *> sps *> (intVal <* sps <* char(')') <* sps)).? ~ // optinal queue size
      (char('{') *> sps *>
        (knownrebecs <* sps).? ~ //
        (statevars <* sps).? ~
        (msgsrv.repSep0(sps) <* sps <* char('}'))
    )).map(x => x._1._1 ->
          ReactiveClass(x._1._2,
                        x._2._1._1.getOrElse(Nil),
                        x._2._1._2.getOrElse(Nil),
                        Map("initial"->Msgsrv(Nil,Skip))++x._2._2.toMap))

  def knownrebecs: P[List[QVar]] =
    string("knownrebecs") *> sps *> blockQVar

  def statevars: P[List[QVar]] =
    string("statevars") *> sps *> blockQVar

  def blockQVar: P[List[QVar]] =
    ((char('{') *> sps *> (qvar <* ending).repSep0(sps)) <* sps <* char('}'))
      .map(_.toList)

  def msgsrv: P[(String,Msgsrv)] =
    (string("msgsrv") *> fsps *>
      (anyName <* sps) ~ // methodname
        ((char('(') *> sps *> qvar.repSep0(sps ~ char(',') ~ sps)) <* sps <* char(')') <* sps) ~ // args
        ((char('{') *> sps *> statement.?) <* sps <* char('}')) // body
      ).map(x => x._1._1 -> Msgsrv(x._1._2.toList,x._2.getOrElse(Skip)))

  def qvar: P[QVar] =
    (anyName ~ (sps *> anyName))
      .map(x => QVar(x._2,x._1))

  private def ending: P[Unit] =
    sps.with1 *> char(';')

  def statement: P[Statement] = P.recursive(stmRec =>
    def basicStm: P[Statement] =
      skip | ite | delay | ite | call.backtrack | assign.backtrack | choice.backtrack | newInst
    //      contract | skip | ite | whilec | assert | assign

    def ite: P[ITE] =
      (((string("if") *> sps *> char('(') *> sps *> expr2) <* sps <* char(')') <* sps) ~ // bool
        (commBlock <* sps) ~ // then
        (string("else") *> sps *> commBlock).?) // else
        .map(x => ITE(x._1._1, x._1._2, x._2.getOrElse(Skip)))

    def commBlock =
      (char('{') ~ sps) *> stmRec.?.map(_.getOrElse(Skip)) <* (sps ~ char('}')) |
        skip | call.backtrack | assign.backtrack | choice.backtrack | newInst

    def assign: P[Assign] =
    //      (varName ~ (string(":=")|char('=')).surroundedBy(sps) ~ iexpr)
      (anyName ~ (string("=") | char('=')).surroundedBy(sps) ~ expr2 <* (sps ~ char(';')))
        .map(x => Assign(x._1._1, x._2))
    //    def contract: P[Command] = // contracts now only in the outside, after ';' is at the end of every assignment.
    //      (invariant~commRec.surroundedBy(sps)~invariant)
    //        .map(x => Contract(x._1._1, x._1._2, x._2))

//    def seqOp =
//      char(';').as(Seq.apply)
//
//    def seqOp2 =
//      sps.as(Seq.apply)

    //    listSep(basicCommand, seqOp2)
    //    listSepRep(basicCommand, char(';').surroundedBy(sps), Seq.apply)
    listSepRep(basicStm, sps, Seq.apply)
  )

  def skip: P[Skip.type] =
  //      string("skip").as(Skip)
    (string("skip") ~ ending).as(Skip)

  def call: P[Call] =
    ((anyName <* sps) ~ // rebeca name
      (char('.') *> anyName <* sps) ~ // method name
      ((char('(') *> sps *> expr2.repSep0(sps~char(',')~sps)) <* sps <* char(')') <* sps) ~
      (string("after") *> sps *> char('(') *> (expr2 <* sps <* char(')') <* sps)).? ~
      (string("deadline") *> sps *> char('(') *> (expr2 <* sps <* char(')'))).? <* ending
    ).map(x => Call(x._1._1._1._1,x._1._1._1._2,x._1._1._2,x._1._2,x._2)) // todo: "after" and "deadline"

  def choice: P[Choice] =
    ((anyName <* sps) ~ // var name
      (char('=') *> sps *> char('?') *> sps *> char('(') *> sps *>
        (expr2.repSep(sps ~ char(',') ~ sps) <* sps <* char(')') <* ending)) // args
      ).map(x => Choice(x._1,x._2.toList))

  def newInst: P[NewReb] =
    ((anyName <* sps <* // var name
       char('=') <* sps <*
       string("new") <* sps) ~ // new keyword
     (className <* sps) ~ // className
        ((char('(') *> sps *> varName.repSep0(sps ~ char(',') ~ sps)) <* sps <* char(')')) ~ // known rebecs
        ((sps *> char(':') *> sps *> char('(') *>
          //(digits.map(s => Data.N(s.toInt)) | string("true").as(Data.B(true)) | string("false").as(Data.B(false)))
          expr2
            .repSep0(sps ~ char(',') ~ sps)) <*
          sps <* char(')') <* ending ) // queue sizes
//        ).map(x => InstanceDecl(x._1._1._1,x._1._1._2,x._1._2,x._2))
      ).map(x => NewReb(InstanceDecl(x._1._1._2, x._1._1._1, x._1._2, x._2)))

  def delay:P[Delay] =
    (string("delay") *> sps *> char('(') *> sps *> (expr2 <* sps <* char(')') <* ending )
    ).map(Delay.apply)


//  def expr: P[Expr] =
////    varName.map(GVar(_)) | bexpr.backtrack | iexpr
//    (bexpr. backtrack | iexpr)
//      .map { case BVar(v) => GVar(v); case e => e }
////    (varName <* sps <* char(until)).map(Program.GVar.apply).backtrack |
////      bexpr <* sps <* char(until) |
////      iexpr <* sps <* char(until)

  def expr2: P[Expr2] =P.recursive[Expr2](exprRec =>
    // literal: constant, variable, parenthesis, or !literal
    def lit: P[Expr2] = P.recursive(litR =>
      string("true").as(Expr2.B(true)) |
      string("false").as(Expr2.B(false)) |
      digits.map(x => Expr2.N(x.toInt)) |
      (char('-')*>digits).map(x => Expr2.N(x.toInt * -1)) |
//      (string("new") *> sps *> call).map(c => Expr2.NewReb(c)) |
      anyName.map(Expr2.Var.apply) |
      char('(') *> exprRec <* char(')') |
      (char('!') *> litR).map(x => Expr2.Func("not",List(x)))
    )

    def op1: P[String] = (string("||") | string("\\/")).as("||")
    def op2: P[String] = (string("&&") | string("/\\")).as("&&")
    def op3: P[String] = oneOf("<>=".toList.map(char)).rep.string | string("!=").string
    def op4: P[String] = oneOf("+-".toList.map(char)).string
    def op5: P[String] = oneOf("*/%".toList.map(char)).string


    def combine(x:(Expr2,Option[(String,Expr2)])): Expr2 =
      if x._2.isDefined then Expr2.Infix(x._2.get._1, x._1, x._2.get._2) else x._1

    def infix(p:P[Expr2],ops:P[String]): P[Expr2] = P.recursive(rec =>
      ((p <* sps) ~ ((ops <* sps) ~ rec).?).map(combine))

    def orP: P[Expr2]   = infix(andP,op1)
    def andP: P[Expr2]  = infix(ineqP,op2)
    def ineqP: P[Expr2] = infix(sumP,op3)
    def sumP: P[Expr2]  = infix(multP,op4)
    def multP: P[Expr2] = infix(lit,op5)

    orP
  )


  ////////////////////
  /// DEPRECATED? //
  ///////////////////
//  def toBExpr(e:Expr): BExpr = e match
//    case b:BExpr => b
//    case GVar(name) => BVar(name)
//    case IVar(name) => BVar(name)
//    case _ => sys.error(s"Expected boolean but found int: $e")
//
//  def bexpr: P[BExpr] = expr.map(toBExpr)
//  /** (Recursive) Parser for a boolean expression */
//  def expr: P[Expr] = P.recursive[Expr](exprRec =>
//    def lit: P[Expr] = P.recursive(litR =>
//      string("true").as(BTrue) |
//        string("false").as(BFalse) |
////        varName.map(BVar.apply) |
//        (char('!') *> litR).map(x => Not(toBExpr(x))) |
//        ineq.backtrack |
//        char('(') *> exprRec <* char(')')
//    )
//
//    def insideBrackets: P[Expr] =
//      exprRec.backtrack | ineq
//
//    def op: P[(Expr, Expr) => BExpr] =
//      string("<=").as((x: Expr, y: Expr) => Or(Less(x, y), Eq(x, y))) |
//        string(">=").as((x: Expr, y: Expr) => Or(Greater(x, y), Eq(x, y))) |
//        string("!=").as((x: Expr, y: Expr) => Not(Eq(x, y))) |
//        char('<').as(Less.apply) |
//        char('>').as(Greater.apply) |
//        string("==").as(Eq.apply)
//
//    def ineq:P[Expr] =
//      (varOrIexpr ~ (op.surroundedBy(sps) ~ varOrIexpr).?)
//        .map(x => if x._2.isDefined then x._2.get._1(x._1,x._2.get._2) else x._1)
////          .match {
////          case IVar(n) => BVar(n)
////          case e => sys.error(s"unexpected integer expression ${x._1}")
////        })
////        .map(x => x._1._2(x._1._1, x._2))
//
//    def varOrIexpr:P[Expr] =
//      iexpr.map(_ match
//        case IVar(n) => GVar(n)
//        case i => i
//      )
//
//    def or: P[(Expr, Expr) => BExpr] =
//      (string("||") | string("\\/")).map(_ => (x,y) => Or(toBExpr(x),toBExpr(y)))
//
//    def and: P[(Expr, Expr) => BExpr] =
//      (string("&&") | string("/\\")).map(_ => (x,y) => And(toBExpr(x),toBExpr(y)))
//
//    listSep(listSep(lit, and), or)
//  )
//
//  /** (Recursive) Parser for an integer expression */
//  def iexpr: P[IExpr] = P.recursive(iexprRec =>
//    def lit: P[IExpr] =
//      char('(') *> iexprRec.surroundedBy(sps) <* char(')') |
//        digits.map(x => N(x.toInt)) |
//        anyName.map(IVar.apply)
//
//    def pow: P[(IExpr, IExpr) => IExpr] =
//      string("^").map(_ => Power.apply)
//
//    def mult: P[(IExpr, IExpr) => IExpr] =
//      string("*").map(_ => Times.apply)
//
//    def plusminus: P[(IExpr, IExpr) => IExpr] =
//      string("+").as(Plus.apply) |
//        string("-").as(Minus.apply)
//
//    listSep(listSep(listSep(lit, pow), mult), plusminus)
//  )


  /// Auxiliary parser combinators

  /** Non-empty list of elements with a binary operator */
//  def listSep[A](elem: P[A], op: P0[(A, A) => A]): P[A] =
//    ((elem <* sps) ~ (op.backtrack.with1 ~ (sps.with1 *> elem)).rep0)
//      .map(x =>
//        val pairlist = x._2
//        val first = x._1;
//        pairlist.foldLeft(first)((rest, pair) => pair._1(rest, pair._2))
//      )

  /** Pair of elements with a separator */
//  def binary[A, B](p1: P[A], op: String, p2: P[B]): P[(A, B)] =
//    (p1 ~ string(op).surroundedBy(sps) ~ p2).map(x => (x._1._1, x._2))

  /** Similar to `listSep`, but using native repSep that is more eager (less backtrack). */
  def listSepRep[A](elem: P[A], sep: P0[_], join: (A, A) => A): P[A] =
    elem.repSep(sep)
      .map(ls => ls.tail.fold(ls.head)(join(_, _)))


  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////
  object Examples:
    val ex1 =
      """x:=28; while(x>1) do x:=x-1"""
