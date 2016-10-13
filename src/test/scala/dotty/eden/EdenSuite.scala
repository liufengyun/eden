package dotty.eden

import org.scalatest.FunSuite
import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Contexts.ContextBase
import scala.meta.dialects.Dotty

trait EdenSuite extends FunSuite {
  implicit val ctx = (new ContextBase).initialCtx

  def dottyParse(code: String): untpd.Tree = {
    val (_, stats) = new Parser(new SourceFile("<meta>", code.toCharArray)).templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  def checkUntpd(code: String, expect: m.Stat) = {
    test(code) {
      val dTree: untpd.Tree = dottyParse(code)
      var convertedTree: m.Tree = Convert.toMTreeUntpd(dTree)
      assert(expect.structure == convertedTree.structure)
    }
  }

  def checkUntpd(code: String, verbose: Boolean = false): Unit = {
    test(code) {
      import scala.meta._
      val mTree: m.Tree = code.parse[m.Stat].get
      val dTree: untpd.Tree = dottyParse(code)
      var convertedTree: m.Tree = null

      try { convertedTree = Convert.toMTreeUntpd(dTree) } finally {
        if (convertedTree == null || mTree.structure != convertedTree.structure || verbose)
          debug
      }

      def debug = {
        println("<------------")
        println("code:" + code)
        println("dotty:" + dTree)
        println("meta:" + mTree.structure)
        if (convertedTree != null) println("conv:" + convertedTree.structure)
        println("------------>")
      }

      assert(mTree.structure == convertedTree.structure)
    }
  }
}
