package rebecaos.backend

import RebecEnv.*
import rebecaos.syntax.Program.Msgsrv
import rebecaos.backend.Eval.Data
import scala.annotation.targetName

case class RebecEnv(vars: Valuation, rebs: KnownReb, meth: Methods, clazz: String):
  @targetName("addValuation")
  def ++(vars2: Valuation) = RebecEnv(vars ++ vars2, rebs, meth, clazz)
  @targetName("addAssignment")
  def +(vd: (String, Data)) = RebecEnv(vars + vd, rebs, meth, clazz)
  @targetName("addRebecs")
  def ++(rebs2: Iterable[(String, String)]) = RebecEnv(vars, rebs ++ rebs2, meth, clazz)
  @targetName("addRebec")
  def +(kn: (String, String)) = RebecEnv(vars, rebs + kn, meth, clazz)
  def now: Int = vars.getOrElse("now", 0) match
    case Data.N(i) => i
    case b => sys.error(s"variable 'now' should be an int, but it is '$b'.")

object RebecEnv:
  private var seed = 0;
  def restart: Unit = seed = 0
  def newVar: String =
    seed += 1
    s"v${seed - 1}"
  def empty: RebecEnv = RebecEnv(Map(), Map(), Map(), "")
  //  type Data = Boolean|Int
  type Valuation = Map[String,Data]
  type KnownReb  = Map[String,String] // from local names to system names
  type Methods   = Map[String,Msgsrv]