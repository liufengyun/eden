package scala.meta

import scala.{meta => m}
import dotty.tools.dotc._
import ast.untpd
import core.Contexts.Context
import util.SourceFile
import parsing.Parsers.Parser

package object eden {
  import convert._

  implicit def toMeta(tree: untpd.Tree, isTerm: Boolean = true)(implicit ctx: Context): m.Tree = {
    new Scala2MetaConvert(if (isTerm) TermMode else TypeMode, ExprLoc).toMTree(tree)
  }

  implicit def toMeta(tree: untpd.Tree)(implicit ctx: Context): m.Tree = toMeta(tree, isTerm = true)

  def toScala(mtree: m.Tree)(implicit ctx: Context): untpd.Tree = {
    val parser = new Parser(new SourceFile("<meta>", mtree.syntax.toArray))
    val (_, stats) = parser.templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

}

