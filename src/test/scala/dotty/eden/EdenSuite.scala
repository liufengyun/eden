package dotty.eden

import org.scalatest.FunSuite
import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Contexts.ContextBase

trait EdenSuite extends FunSuite {
  implicit val ctx = (new ContextBase).initialCtx

  def dottyParse(code: String): untpd.Tree = {
    val (_, stats) = new Parser(new SourceFile("<meta>", code.toCharArray)).templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  def checkUntpd(code: String): Unit = {
    test(code) {
      import scala.meta._
      val mTree: m.Tree = code.parse[m.Stat].get
      val dTree: untpd.Tree = dottyParse(code)
      val convertedTree: m.Tree = Convert.toMTreeUntpd(dTree)
      println("meta:" + mTree.structure)
      assert(mTree.structure == convertedTree.structure)
    }
  }
}
