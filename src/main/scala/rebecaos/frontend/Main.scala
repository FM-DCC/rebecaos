package rebecaos.frontend

import caos.frontend.Site.initSite
import rebecaos.backend.Semantics.St
import rebecaos.syntax.Program
import rebecaos.syntax.Program.System

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
    initSite[St](CaosConfig)
}