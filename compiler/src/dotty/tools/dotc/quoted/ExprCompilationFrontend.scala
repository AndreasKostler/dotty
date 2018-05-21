package dotty.tools.dotc.quoted

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.{EmptyFlags, Method}
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Scopes.{EmptyScope, newScope}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.typer.FrontEnd
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{Path, PlainFile}

import scala.quoted.Expr

//** Frontend that receives a scala.quoted.Expr as input to emit a compiled class for it */
class ExprCompilationFrontend(outputClassName: TypeName) extends FrontEnd {
  import tpd._

  override def isTyper = false

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    units.map {
      case exprUnit: ExprCompilationUnit =>
        val tree = inClass(exprUnit.expr)
        val source = new SourceFile("", Seq())
        CompilationUnit.mkCompilationUnit(source, tree, forceTrees = true)
    }
  }

  /** Places the contents of expr in a compilable tree for a class
    *  with the following format.
    *  `package __root__ { class ' { def apply: Any = <expr> } }`
    */
  private def inClass(expr: Expr[_])(implicit ctx: Context): Tree = {
    val pos = Position(0)
    val assocFile = new PlainFile(Path("<quote>"))

    val cls = ctx.newCompleteClassSymbol(defn.RootClass, outputClassName, EmptyFlags,
      defn.ObjectType :: Nil, newScope, coord = pos, assocFile = assocFile).entered.asClass
    cls.enter(ctx.newDefaultConstructor(cls), EmptyScope)
    val meth = ctx.newSymbol(cls, nme.apply, Method, ExprType(defn.AnyType), coord = pos).entered

    val quoted = PickledQuotes.quotedExprToTree(expr)(ctx.withOwner(meth))

    val run = DefDef(meth, quoted)
    val classTree = ClassDef(cls, DefDef(cls.primaryConstructor.asTerm), run :: Nil)
    PackageDef(ref(defn.RootPackage).asInstanceOf[Ident], classTree :: Nil).withPos(pos)
  }
}
