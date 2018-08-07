package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases._

/** Retain trees of the compilation units if -Yretain-trees is set */
class RetainTrees extends Phase {
  import tpd._

  def phaseName: String = "RetainTrees"

  def run(implicit ctx: Context): Unit = {
    if (ctx.settings.YretainTrees.value)
      retainTopLevelClassTrees(ctx.compilationUnit.tpdTree)
  }

  private def retainTopLevelClassTrees(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case PackageDef(_, stats) => stats.foreach(retainTopLevelClassTrees)
    case tree: TypeDef => tree.symbol.asClass.treeOrProvider = tree
    case _ =>
  }

}
