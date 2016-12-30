package scala.meta.eden

import org.scalatest.FunSuite
import scala.{meta => m}
import dotty.tools.dotc._
import ast.untpd
import parsing.Parsers.Parser
import util.SourceFile
import core.Contexts.ContextBase
import scala.meta.dialects.Dotty

trait EdenSuite extends FunSuite {
  implicit val ctx = (new ContextBase).initialCtx.fresh

  implicit def dottyParse(code: String): untpd.Tree = {
    val (_, stats) = new Parser(new SourceFile("<meta>", code.toCharArray)).templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  implicit def metaParse(code: String): m.Stat = {
    import scala.meta._
    code.parse[m.Stat].get
  }
}
