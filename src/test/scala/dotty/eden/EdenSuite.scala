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

  implicit def dottyParse(code: String): untpd.Tree = {
    val (_, stats) = new Parser(new SourceFile("<meta>", code.toCharArray)).templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  implicit def metaParse(code: String): m.Stat = {
    import scala.meta._
    code.parse[m.Stat].get
  }

  def syntactic(code: String, expect: m.Stat) = {
    test(code) {
      val dTree: untpd.Tree = code
      var convertedTree: m.Tree = dTree
      assert(expect.structure == convertedTree.structure)
    }
  }

  def syntactic(code: String, verbose: Boolean = false): Unit = {
    test(code) {
      val mTree: m.Tree = code
      val dTree: untpd.Tree = code
      var convertedTree: m.Tree = null

      try { convertedTree = dTree } finally {
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
