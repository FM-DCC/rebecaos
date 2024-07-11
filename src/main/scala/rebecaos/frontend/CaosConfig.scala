package rebecaos.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import rebecaos.backend.*
import rebecaos.backend.Semantics.*
import rebecaos.syntax.Program.System
import rebecaos.syntax.{Program, Show}

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[St]:
  val name = "Animator of Rebeca's semantics"
  override val languageName: String = "Input Rebeca program"

  /** Parser, converting a string into a System in rebecaOS */
  val parser =
//    str => (rebecaos.syntax.Parser.parseProgram(str),Map())
    str => initSt(rebecaos.syntax.Parser.parseProgram(str))

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "Sarir-basic" -> "reactiveclass Example {\n\tknownrebecs { Example ex;}\n\tstatevars { int counter; }\n\tmsgsrv initial() {\n    counter=0;\n    ex.add(0);}\n\tmsgsrv add(int a) {\n\t\tif ( counter < 100) \n\t\t\t{counter = counter + 1;}\n  }\n}\n\nmain {\n\tExample ex1( ex2):();\n\tExample ex2( ex1):();\n}"
      -> "Simple example of a Rebeca program, taken from the Sarir paper.",
    "Prod-Cons" -> "reactiveclass Producer {\n\tknownrebecs {\n\t\tConsumer consumer;\n\t}\n\tstatevars {\n\t\tbyte p;\n\t}\n\tmsgsrv initial() {\n\t\tself.produce();\n\t}\n\tmsgsrv produce() {\n\t\t// produce data\n\t\tp=?(1,2,3,4);\n\t\tconsumer.consume(p);\n\t\tself.produce();\n\t}\n}\n\nreactiveclass Consumer {\n\tknownrebecs {\n\t}\n\tstatevars {\n\t\tbyte p;\n\t}\n\tmsgsrv initial() {\n\t}\n\tmsgsrv consume(byte data) {\n\t\t// consume data\n\t\tp = data;\n\t}\n}\n\nmain {\n\tProducer producer(consumer):();\n\tConsumer consumer():();\n}"
      -> "Producer-consumer example from FMCO 2006's paper from Marjan Sirjani",
    "Prod-Cons (dyn)" -> "reactiveclass Producer {\n\tknownrebecs {\n\t\tConsumer consumer;\n\t}\n\tstatevars {\n\t\tbyte p;\n    Producer newProducer;\n\t}\n\tmsgsrv initial() {\n\t\tself.produce();\n\t}\n\tmsgsrv produce() {\n\t\t// produce data\n\t\tp=?(1,2,3,4);\n    if (p==3) {\n\t\t\tnewProducer = new Producer(consuler):();\n    }\n    consumer.consume(p);\n\t\tif(p!=4){\n    \tself.produce();\n    }\n\t}\n}\n\nreactiveclass Consumer {\n\tknownrebecs {\n\t}\n\tstatevars {\n\t\tbyte p;\n\t}\n\tmsgsrv initial() {\n\t}\n\tmsgsrv produce(byte data) {\n\t\t// consume data\n\t\tp = data;\n\t}\n}\n\nmain {\n\tProducer producer(consumer):();\n\tConsumer consumer():();\n}"
      -> "(Unsupported) Dynamic version of the producer-consumer example from FMCO 2006's paper from Marjan Sirjani",
    "Timed-ticket" -> "reactiveclass TicketService {\n  knownrebecs {\n    Agent a;\n  }\n  statevars {\n    int issueDelay;\n  }\n  msgsrv initial(int myDelay) {\n    issueDelay = myDelay;\n  }\n  msgsrv requestTicket() {\n    delay(issueDelay);\n    a.ticketIssued (1);\n  }\n}\nreactiveclass Agent {\n  knownrebecs {\n    TicketService ts;\n    Customer c;\n  }\n  msgsrv requestTicket() {\n    ts.requestTicket()\n      deadline (5);\n  }\n\n  msgsrv ticketIssued(byte id) {\n    c.ticketIssued(id);\n  }\n}\nreactiveclass Customer {\n  knownrebecs {\n    Agent a;\n  }\n  msgsrv initial() {\n    self.try();\n  }\n  msgsrv try() {\n    a.requestTicket();\n  }\n  msgsrv ticketIssued(byte id) {\n    self.try() after(30);\n  }\n}\nmain {\n  Agent a(ts, c):();\n  TicketService ts(a):(3);\n  Customer c(a):();\n}"
      -> "Ticket Service from an SCP'25 paper on Timed Rebeca",
    "Dining Philosophers" -> "reactiveclass Philosopher(3)\n{\n  knownrebecs\n  {\n    Fork forkL;\n    Fork forkR;\n  }\n  statevars\n  {\n    boolean eating;\n    boolean fL;\n    boolean fR;\n  }\n  msgsrv initial()\n  {\n    fL = false;\n    fR = false;\n    eating = false;\n    self.arrive();\n  }\n\n  msgsrv arrive()\n  {\n    forkL.request();\n  }\n\n  msgsrv permit()\n  {\n    if (sender == forkL) {\n      if (!fL) {\n        fL = true;\n        forkR.request();\n      }\n    }\n    else {\n      if (fL && !(fR)) {\n        fR = true;\n        self.eat();\n      }\n      // else discard the message\n    }\n  }\n\n  msgsrv eat()\n  {\n    eating = true;\n    self.leave();\n  }\n\n  msgsrv leave()\n  {\n    fL = false;\n    fR = false;\n    eating = false;\n    forkL.release();\n    forkR.release();\n    self.arrive();\n  }\n}\n\nreactiveclass Fork(3)\n{\n  knownrebecs\n  {\n    Philosopher philL;\n    Philosopher philR;\n  }\n  statevars\n  {\n    boolean lAssign;  \n    boolean rAssign;  \n    boolean leftReq;\n    boolean rightReq;\n  }\n  msgsrv initial()\n  { \n    lAssign = false;\n    rAssign = false;\n    leftReq = false;\n    rightReq = false;\n  }\n\n  msgsrv request()\n  {\n    if (sender == philL) {\n      if (!leftReq) {\n        leftReq = true;\n        if (!rAssign) {\n          lAssign = true;\n          philL.permit();\n        }\n      } \n      // else discard message\n    }\n    else {\n      if (!rightReq) {\n        rightReq = true;\n        if (!lAssign) {\n          rAssign = true;\n          philR.permit();\n        }\n      }\n      // else discard message\n    }\n  }\n  msgsrv release()\n  {\n    if (sender == philL && lAssign){\n      leftReq = false;\n      lAssign = false;\n      if (rightReq) {\n        rAssign=true;\n        philR.permit();\n      }\n    }\n    if (sender == philR && rAssign){\n      rAssign = false;\n      rightReq = false;\n      if (leftReq) {\n        lAssign=true;\n        philL.permit();\n      }\n    }\n  }\n}\n\nmain\n{\n  Philosopher phil0(fork0, fork2):();\n  Philosopher phil1(fork0, fork1):();\n  Philosopher phil2(fork1, fork2):();\n\n  Fork fork0(phil0, phil1):();\n  Fork fork1(phil1, phil2):();\n  Fork fork2(phil2, phil0):();\n}"
      -> "Dining philosophers, taken from http://rebeca-lang.org/Rebeca.",
    "Trains" -> "reactiveclass BridgeController(5) {\n  knownrebecs {\n    Train t1;\n    Train t2;\n  }\n\n  statevars {\n    boolean isWaiting1;\n    boolean isWaiting2;\n    boolean isOnBridge1;\n    boolean isOnBridge2;\n  }\n\n  msgsrv initial() {\n    isOnBridge1 = false;  // red\n    isOnBridge2 = false;  // red\n    isWaiting1 = false;\n    isWaiting2 = false;\n  }\n  \n  msgsrv Arrive() {\n    if (sender == t1){\n      if (isOnBridge2 == false) {\n        isOnBridge1 = true; // green\n        t1.YouMayPass();\n      }\n      else {\n        isWaiting1 = true;\n      }\n    }\n    else {\n      if (isOnBridge1 == false){\n        isOnBridge2 = true; // green\n        t2.YouMayPass();\n      }\n      else{\n        isWaiting2 = true;\n      }\n    }\n  }\n  \n  msgsrv Leave() {\n    if (sender == t1) {\n      isOnBridge1 = false;  // red\n      if (isWaiting2){\n        isOnBridge2 = true;\n        t2.YouMayPass();\n        isWaiting2 = false;\n      }\n    } else {\n      isOnBridge2 = false;  // red\n      if (isWaiting1) {\n        isOnBridge1 = true;\n        t1.YouMayPass();\n        isWaiting1 = false;\n      }\n    }\n  }\n}\n\nreactiveclass Train(3) {\n  knownrebecs {\n    BridgeController controller;\n  }\n\n  statevars {\n    boolean onTheBridge;\n  }\n\n  msgsrv initial() {\n    onTheBridge = false;\n    self.Passed();\n  }\n\n  msgsrv YouMayPass() {\n    onTheBridge = true;\n    self.Passed();\n  }\n  \n  msgsrv Passed() {\n    onTheBridge = false;\n    controller.Leave();\n    self.ReachBridge();\n  }\n\n  msgsrv ReachBridge() {\n    controller.Arrive();\n  }\n}\n\nmain {\n  Train train1(theController):();\n  Train train2(theController):();\n  BridgeController theController(train1, train2):();\n}"
      -> "Two trains and a controller, taken from http://rebeca-lang.org/Rebeca.",
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
    "View pretty data" -> view[St](_.toString, Code("haskell")).moveTo(0),
//    "View structure" -> view(Show.mermaid, Mermaid),
     "Run semantics" -> steps((e:St)=>e, Semantics, Show.apply, _.toString, Text).expand,
     "Build LTS" -> lts((e:St)=>e, Semantics, Show.short, _.toString),
     "Build LTS (explore)" -> ltsExplore(e=>e, Semantics, Show.short, _.toString),
//    "Find strong bisimulation (given a program \"A ~ B\")" ->
//      compareStrongBisim(Semantics, Semantics,
//        (e: System) => System(e.defs, e.main, None),
//        (e: System) => System(e.defs, e.toCompare.getOrElse(Program.Term.End), None),
//        Show.justTerm, Show.justTerm, _.toString),
  )

  //// Documentation below

  override val footer: String =
    """Simple animator of rebecaOS, meant to exemplify the
      | CAOS libraries, used to generate this website. Source code available online:
      | <a target="_blank" href="https://github.com/arcalab/CAOS">
      | https://github.com/arcalab/CAOS</a> (CAOS).""".stripMargin

  private val sosRules: String =
    """The operational rules that we use to reduce programs are provided below.
      |These are explained, e.g. in
      |<a href="http://dx.doi.org/10.1016/j.scico.2014.07.005" target="_blank">this SCP publication (2014)</a>.
      |<pre>
      |WHEN
      |  (σ_ri(m), // statement
      |   σ_ri[now=max(TT,σ_ri(now)),[arg=v],sender=rj], // env
      |   Env,    // USELESS
      |   B)      // outgoing messages // NOT NEEDED here
      |  EVALUATES TO
      |  (σ'_ri,   // new env
      |   Env',   // ALWAYS Env (without dynamic "new" creation)
      |   B')     // new/updated outgoing messages
      |IF
      |  TT≤min(B) ∧
      |  σ_ri(now)≤DL ∧
      |  (ri,m(v),rj,TT,DL) notin B ∧
      |  σ_ri notin Env
      |THEN
      |  ({σri}∪Env,
      |   {(ri,m(v),rj,TT,DL)}∪B)
      |  →
      |  ({σ'_ri} ∪ Env',
      |   B')
      |</pre>""".stripMargin

  override val documentation: Documentation = List(
    languageName -> "More information on the syntax of rebecaOS" ->
      """The syntax and timed (static) semantics for a Rebeca program can be found, e.g., in
        |<a href="http://dx.doi.org/10.1016/j.scico.2014.07.005" target="_blank">this SCP publication (2014)</a>.
        |""".stripMargin,
    "Build LTS" -> "More information on the operational rules used here" -> sosRules,
    "Build LTS (explore)" -> "More information on the operational rules used here" -> sosRules,
    "Run semantics" -> "More information on the operational rules used here" -> sosRules,
  )
