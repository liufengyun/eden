package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}

object Convert {
  def toMTree(tree: tpd.Tree): m.Tree = ???
  def toMTree(tree: untpd.Tree): m.Tree = ???
}

