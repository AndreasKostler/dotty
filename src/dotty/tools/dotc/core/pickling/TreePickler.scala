package dotty.tools
package dotc
package core
package pickling

import ast.Trees._
import PickleFormat._
import core._
import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._, StdNames.tpnme
import collection.mutable
import TastyBuffer._

class TreePickler(pickler: TastyPickler) {
  val buf = new TreeBuffer
  pickler.newSection("ASTs", buf)
  import buf._
  import pickler.nameBuffer.nameIndex
  import ast.tpd._

  private val symRefs = new mutable.HashMap[Symbol, Addr]
  private val forwardSymRefs = new mutable.HashMap[Symbol, List[Addr]]
  private val pickledTypes = new java.util.IdentityHashMap[Type, Any] // Value type is really Addr, but that's not compatible with null

  private def withLength(op: => Unit) = {
    val lengthAddr = reserveRef(relative = true)
    op
    fillRef(lengthAddr, currentAddr, relative = true)
  }
  
  private var makeSymbolicRefsTo: Symbol = NoSymbol

  /** All references to members of class `sym` are pickled
   *  as symbolic references. Used to pickle the self info of a class.
   *  Without this precaution we get an infinite cycle when unpickling pos/extmethods.scala
   *  The problem arises when a self type of a trait is a type parameter of the same trait.
   */
  private def withSymbolicRefsTo[T](sym: Symbol)(op: => T): T = {
    val saved = makeSymbolicRefsTo
    makeSymbolicRefsTo = sym
    try op
    finally makeSymbolicRefsTo = saved
  }
  
  def preRegister(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: MemberDef => 
      if (!symRefs.contains(tree.symbol)) symRefs(tree.symbol) = NoAddr
    case _ =>
  }

  def registerDef(sym: Symbol): Unit = {
    symRefs(sym) = currentAddr
    forwardSymRefs.get(sym) match {
      case Some(refs) =>
        refs.foreach(fillRef(_, currentAddr, relative = false))
        forwardSymRefs -= sym
      case None =>
    }
  }

  private def pickleName(name: Name) = writeNat(nameIndex(name).index)
  private def pickleName(name: TastyName) = writeNat(nameIndex(name).index)
  private def pickleNameAndSig(name: Name, sig: Signature) = {
    val Signature(params, result) = sig
    pickleName(TastyName.Signed(nameIndex(name), params.map(nameIndex), nameIndex(result)))
  }

  private def pickleSymRef(sym: Symbol)(implicit ctx: Context) = symRefs.get(sym) match {
    case Some(label) =>
      if (label != NoAddr) writeRef(label) else pickleForwardSymRef(sym)
    case None =>
      ctx.log(i"pickling reference to as yet undefined $sym in ${sym.owner}", sym.pos)
      pickleForwardSymRef(sym)
  }
  
  private def pickleForwardSymRef(sym: Symbol)(implicit ctx: Context) = {
    val ref = reserveRef(relative = false)
    assert(!sym.is(Flags.Package), sym)
    forwardSymRefs(sym) = ref :: forwardSymRefs.getOrElse(sym, Nil)  
  }

  def pickle(trees: List[Tree])(implicit ctx: Context) = {
    
    def qualifiedName(sym: Symbol): TastyName =
      if (sym.isRoot || sym.owner.isRoot) TastyName.Simple(sym.name.toTermName)
      else TastyName.Qualified(nameIndex(qualifiedName(sym.owner)), nameIndex(sym.name))

    def pickleConstant(c: Constant): Unit = c.tag match {
      case UnitTag =>
        writeByte(UNITconst)
      case BooleanTag =>
        writeByte(if (c.booleanValue) TRUEconst else FALSEconst)
      case ByteTag =>
        writeByte(BYTEconst)
        writeInt(c.byteValue)
      case ShortTag =>
        writeByte(SHORTconst)
        writeInt(c.shortValue)
      case CharTag =>
        writeByte(CHARconst)
        writeNat(c.charValue)
      case IntTag =>
        writeByte(INTconst)
        writeInt(c.intValue)
      case LongTag =>
        writeByte(LONGconst)
        writeLongInt(c.longValue)
      case FloatTag =>
        writeByte(FLOATconst)
        writeInt(java.lang.Float.floatToRawIntBits(c.floatValue))
      case DoubleTag =>
        writeByte(DOUBLEconst)
        writeLongInt(java.lang.Double.doubleToRawLongBits(c.doubleValue))
      case StringTag =>
        writeByte(STRINGconst)
        writeNat(nameIndex(c.stringValue).index)
      case NullTag =>
        writeByte(NULLconst)
      case ClazzTag =>
        writeByte(CLASSconst)
        pickleType(c.typeValue)
      case EnumTag =>
        writeByte(ENUMconst)
        pickleType(c.symbolValue.termRef)
    }

    def pickleType(tpe0: Type, richTypes: Boolean = false): Unit = try {
      val tpe = tpe0.stripTypeVar
      val prev = pickledTypes.get(tpe)
      if (prev == null) {
        pickledTypes.put(tpe, currentAddr)
        pickleNewType(tpe, richTypes)
      }
      else {
        writeByte(SHARED)
        writeRef(prev.asInstanceOf[Addr])
      }
    } catch {
      case ex: AssertionError =>
        println(i"error when pickling type $tpe0")
        throw ex
    }
      
    def pickleNewType(tpe: Type, richTypes: Boolean): Unit = try { tpe match {
      case ConstantType(value) => 
        pickleConstant(value)
      case tpe: TypeRef if tpe.info.isAlias && tpe.symbol.is(Flags.AliasPreferred) =>
        pickleType(tpe.info.bounds.hi)
      case tpe: WithFixedSym =>
        val sym = tpe.symbol
        if (sym.is(Flags.Package)) {
          writeByte(if (tpe.isType) TYPEREFpkg else TERMREFpkg)
          pickleName(qualifiedName(sym))
        } 
        else if (tpe.prefix == NoPrefix) {
          def pickleRef() = {
            writeByte(if (tpe.isType) TYPEREFdirect else TERMREFdirect)
            pickleSymRef(sym)        
          }
          if (sym is Flags.BindDefinedType) {
            registerDef(sym)
            writeByte(BIND)
            withLength {
              pickleName(sym.name)
              pickleType(sym.info)
              pickleRef()
            }
          }
          else pickleRef()
        }
        else {
          writeByte(if (tpe.isType) TYPEREFsymbol else TERMREFsymbol)
          pickleSymRef(sym); pickleType(tpe.prefix)
        }
      case tpe: TermRefWithSignature =>
        writeByte(TERMREF)
        pickleNameAndSig(tpe.name, tpe.signature); pickleType(tpe.prefix)
      case tpe: NamedType =>
        if (tpe.name == tpnme.Apply && tpe.prefix.argInfos.nonEmpty && tpe.prefix.isInstantiatedLambda)
          // instantiated lambdas are pickled as APPLIEDTYPE; #Apply will 
          // be reconstituted when unpickling.
          pickleType(tpe.prefix)
        else tpe.prefix match {
          case prefix: ThisType if prefix.cls == makeSymbolicRefsTo =>
            pickleType(NamedType.withFixedSym(tpe.prefix, tpe.symbol))
          case _ =>
            writeByte(if (tpe.isType) TYPEREF else TERMREF)
            pickleName(tpe.name); pickleType(tpe.prefix)
        }
      case tpe: ThisType =>
        writeByte(THIS)
        pickleType(tpe.tref)
      case tpe: SuperType =>
        writeByte(SUPERtype)
        withLength { pickleType(tpe.thistpe); pickleType(tpe.supertpe)}
      case tpe: SkolemType =>
        writeByte(SKOLEMtype)
        writeRef(pickledTypes.get(tpe.binder).asInstanceOf[Addr])
      case tpe: RefinedType =>
        val args = tpe.argInfos(interpolate = false)
        if (args.isEmpty) {
          writeByte(REFINEDtype)
          withLength { 
            pickleType(tpe.parent)
            pickleName(tpe.refinedName)
            pickleType(tpe.refinedInfo, richTypes = true) 
          }
        }
        else {
          writeByte(APPLIEDtype)
          withLength { pickleType(tpe.withoutArgs(args)); args.foreach(pickleType(_)) }
        }
      case tpe: TypeAlias =>
        writeByte(TYPEALIAS)
        withLength { 
          pickleType(tpe.alias, richTypes) 
          tpe.variance match {
            case 1 => writeByte(COVARIANT)
            case -1 => writeByte(CONTRAVARIANT)
            case 0 =>
          }
        }
      case tpe: TypeBounds =>
        writeByte(TYPEBOUNDS)
        withLength { pickleType(tpe.lo, richTypes); pickleType(tpe.hi, richTypes) }
      case tpe: AnnotatedType =>
        writeByte(ANNOTATED)
        withLength { pickleTree(tpe.annot.tree); pickleType(tpe.tpe, richTypes) }
      case tpe: AndOrType =>
        writeByte(if (tpe.isAnd) ANDtype else ORtype)
        withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
      case tpe: ExprType =>
        writeByte(BYNAMEtype)
        withLength { pickleType(tpe.underlying) }
      case tpe: MethodType if richTypes =>
        writeByte(METHODtype)
        pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramTypes)
      case tpe: PolyType if richTypes =>
        writeByte(POLYtype)
        pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramBounds)
      case tpe: PolyParam => 
        if (!pickleParamType(tpe))
          // TODO figure out why this case arises in e.g. pickling AbstractFileReader.
          ctx.typerState.constraint.entry(tpe) match {
            case TypeBounds(lo, hi) if lo eq hi => pickleNewType(lo, richTypes)
            case _ => assert(false, s"orphan poly parameter: $tpe")
          }
      case tpe: MethodParam =>
        assert(pickleParamType(tpe), s"orphan method parameter: $tpe")
      case tpe: LazyRef =>
        pickleType(tpe.ref)
      case NoType =>
        writeByte(NOTYPE)
//      case NoPrefix =>    // not sure we need this!
//        writeByte(NOPREFIX)
    }} catch {
      case ex: AssertionError => 
        println(i"error while pickling type $tpe")
        throw ex
    }
    
    def pickleMethodic(result: Type, names: List[Name], types: List[Type]) = 
      withLength {
        pickleType(result, richTypes = true)
        (names, types).zipped.foreach { (name, tpe) =>
          pickleName(name); pickleType(tpe)       
        }
      }
    
    def pickleParamType(tpe: ParamType): Boolean = {
      val binder = pickledTypes.get(tpe.binder)
      val pickled = binder != null
      if (pickled) {
        writeByte(PARAMtype)
        withLength { writeRef(binder.asInstanceOf[Addr]); writeNat(tpe.paramNum) }
      }
      pickled
    }
    
    def pickleTpt(tpt: Tree): Unit = pickleType(tpt.tpe) // TODO correlate with original when generating positions
    
    def pickleTreeUnlessEmpty(tree: Tree): Unit = 
      if (!tree.isEmpty) pickleTree(tree)

    def pickleTree(tree: Tree): Unit = try {
      pickledTrees.put(tree, currentAddr)
      tree match {
      case Ident(name) =>
        tree.tpe match {
          case tp: TermRef => pickleType(tp)
          case _ => 
             writeByte(IDENT)
             pickleName(name)
             pickleType(tree.tpe)
        }
      case This(_) => 
        pickleType(tree.tpe)
      case Select(qual, name) => 
        writeByte(SELECT)
        val sig = tree.tpe.signature
        if (sig == Signature.NotAMethod) pickleName(name)
        else pickleNameAndSig(name, sig)
        pickleTree(qual)
      case Apply(fun, args) =>
        writeByte(APPLY)
        withLength {
          pickleTree(fun)
          args.foreach(pickleTree)
        }
      case TypeApply(fun, args) =>
        writeByte(TYPEAPPLY)
        withLength {
          pickleTree(fun)
          args.foreach(pickleTpt)
        }
      case Literal(const) =>
        pickleConstant(const)
      case Super(qual, mix) =>
        writeByte(SUPER)
        withLength { 
          pickleTree(qual);
          if (!mix.isEmpty) {
            val SuperType(_, mixinType) = tree.tpe
            pickleType(mixinType)
          }
        }
      case New(tpt) =>
        writeByte(NEW)
        pickleTpt(tpt)
      case Pair(left, right) =>
        writeByte(PAIR)
        withLength { pickleTree(left); pickleTree(right) }
      case Typed(expr, tpt) =>
        writeByte(TYPED)
        withLength { pickleTree(expr); pickleTpt(tpt) }
      case NamedArg(name, arg) =>
        writeByte(NAMEDARG)
        withLength { pickleName(name); pickleTree(arg) }
      case Assign(lhs, rhs) =>
        writeByte(ASSIGN)
        withLength { pickleTree(lhs); pickleTree(rhs) }
      case Block(stats, expr) =>
        writeByte(BLOCK)
        stats.foreach(preRegister)
        withLength { pickleTree(expr); stats.foreach(pickleTree) }
      case If(cond, thenp, elsep) =>
        writeByte(IF)
        withLength{ pickleTree(cond); pickleTree(thenp); pickleTree(elsep) }
      case Closure(env, meth, tpt) => 
        writeByte(CLOSURE)
        withLength{ pickleTree(meth); pickleTpt(tpt); env.foreach(pickleTree) }
      case Match(selector, cases) =>
        writeByte(MATCH)
        withLength { pickleTree(selector); cases.foreach(pickleTree) }
      case CaseDef(pat, guard, rhs) =>
        writeByte(CASEDEF)
        withLength { pickleTree(pat); pickleTree(rhs); pickleTreeUnlessEmpty(guard) }
      case Return(expr, from) =>
        writeByte(RETURN)
        withLength { pickleSymRef(from.symbol); pickleTreeUnlessEmpty(expr) }
      case Try(block, cases, finalizer) =>
        writeByte(TRY)
        withLength { pickleTree(block); cases.foreach(pickleTree); pickleTreeUnlessEmpty(finalizer) }
      case Throw(expr) =>
        writeByte(THROW)
        withLength { pickleTree(expr) }
      case SeqLiteral(elems) =>
        writeByte(REPEATED)
        withLength { elems.foreach(pickleTree) }
      case TypeTree(original) =>
        pickleTpt(tree)
      case Bind(name, body) =>
        registerDef(tree.symbol)
        writeByte(BIND)
        withLength { pickleName(name); pickleType(tree.symbol.info); pickleTree(body) }
      case Alternative(alts) =>
        writeByte(ALTERNATIVE)
        withLength { alts.foreach(pickleTree) }
      case UnApply(fun, implicits, patterns) =>
        writeByte(UNAPPLY)
        withLength { 
          pickleTree(fun)
          for (implicitArg <- implicits) {
            writeByte(IMPLICITarg)
            pickleTree(implicitArg)
          }
          pickleType(tree.tpe)
          patterns.foreach(pickleTree) 
        }
      case tree: ValDef =>
        pickleDef(VALDEF, tree.symbol, tree.tpt, tree.rhs)
      case tree: DefDef =>
        def pickleAllParams = {
          pickleParams(tree.tparams)
          for (vparams <- tree.vparamss) {
            writeByte(PARAMS)
            withLength { pickleParams(vparams) }
          }          
        }
        pickleDef(DEFDEF, tree.symbol, tree.tpt, tree.rhs, pickleAllParams)
      case tree: TypeDef =>
        pickleDef(TYPEDEF, tree.symbol, tree.rhs)
      case tree: Template =>
        registerDef(tree.symbol)
        writeByte(TEMPLATE)
        val (params, rest) = tree.body partition {
          case stat: TypeDef => stat.symbol is Flags.Param
          case stat: ValOrDefDef => stat.symbol is Flags.ParamAccessor
          case _ => false
        }
        withLength {
          pickleParams(params)
          tree.parents.foreach(pickleTree)
          val cinfo @ ClassInfo(_, _, _, _, selfInfo) = tree.symbol.owner.info
          if ((selfInfo ne NoType) || !tree.self.isEmpty) {
            writeByte(SELFDEF)
            pickleName(tree.self.name)
            withSymbolicRefsTo(tree.symbol.owner) {
              pickleType {
                cinfo.selfInfo match {
                  case sym: Symbol => sym.info
                  case tp: Type => tp
                }
              }
            }
          }
          pickleStats(tree.constr :: rest)
        }
      case Import(expr, selectors) =>
        writeByte(IMPORT)
        withLength {
          pickleTree(expr)
          selectors foreach {
            case Pair(Ident(from), Ident(to)) => 
              writeByte(RENAMED)
              withLength { pickleName(from); pickleName(to) }
            case Ident(name) =>
              writeByte(IMPORTED)
              pickleName(name)
          }
        }
      case PackageDef(pid, stats) =>
        writeByte(PACKAGE)
        withLength { pickleType(pid.tpe); pickleStats(stats) }
      case EmptyTree =>
        writeByte(EMPTYTREE)
    }}
    catch {
      case ex: AssertionError =>
        println(i"error when pickling tree $tree")
        throw ex
    }

    def pickleDef(tag: Int, sym: Symbol, tpt: Tree, rhs: Tree = EmptyTree, pickleParams: => Unit = ()) = {
      assert(symRefs(sym) == NoAddr)
      registerDef(sym)
      writeByte(tag)
      withLength {
        pickleName(sym.name)
        pickleParams
        tpt match {
          case tpt: TypeTree => pickleTpt(tpt)
          case _ => pickleTree(tpt)
        }
        pickleTreeUnlessEmpty(rhs)
        pickleModifiers(sym)
      }
    }
    
    def pickleParam(tree: Tree): Unit = tree match {
      case tree: ValDef => pickleDef(PARAM, tree.symbol, tree.tpt)
      case tree: DefDef => pickleDef(PARAM, tree.symbol, tree.tpt, tree.rhs)
      case tree: TypeDef => pickleDef(TYPEPARAM, tree.symbol, tree.rhs)      
    }
    
    def pickleParams(trees: List[Tree]): Unit = {
      trees.foreach(preRegister)
      trees.foreach(pickleParam)
    }

    def pickleStats(stats: List[Tree]) = {
      stats.foreach(preRegister)
      stats.foreach(pickleTree)
    }

    def pickleModifiers(sym: Symbol): Unit = {
      import Flags._
      val flags = sym.flags
      val privateWithin = sym.privateWithin
      if (privateWithin.exists) {
        writeByte(if (flags is Protected) PROTECTEDqualified else PRIVATEqualified)
        pickleType(privateWithin.typeRef)
      }
      if (flags is Private) writeByte(PRIVATE)
      if (flags is Protected) if (!privateWithin.exists) writeByte(PROTECTED)
      if (flags is Final) writeByte(FINAL)
      if (flags is Case) writeByte(CASE)
      if (flags is Override) writeByte(OVERRIDE)
      if (flags is Inline) writeByte(INLINE)
      if (flags is JavaStatic) writeByte(STATIC)
      if (flags is Module) writeByte(MODULE)
      if (flags is Local) writeByte(LOCAL)
      if (flags is Synthetic) writeByte(SYNTHETIC)
      if (flags is Artifact) writeByte(ARTIFACT)
      if (flags is Scala2x) writeByte(SCALA2X) 
      if (flags is InSuperCall) writeByte(INSUPERCALL)
      if (sym.isTerm) {
        if (flags is Implicit) writeByte(IMPLICIT)
        if (flags is Lazy) writeByte(LAZY)
        if (flags is AbsOverride) writeByte(ABSOVERRIDE)
        if (flags is Mutable) writeByte(MUTABLE)
        if (flags is Accessor) writeByte(FIELDaccessor)
        if (flags is CaseAccessor) writeByte(CASEaccessor)  
        if (flags is DefaultParameterized) writeByte(DEFAULTparameterized)
        if (flags is DefaultInit) writeByte(DEFAULTinit)
      } else {
        if (flags is Sealed) writeByte(SEALED)
        if (flags is Abstract) writeByte(ABSTRACT)      
        if (flags is Trait) writeByte(TRAIT)
        if (flags is Covariant) writeByte(COVARIANT)
        if (flags is Contravariant) writeByte(CONTRAVARIANT)
      }
      sym.annotations.foreach(pickleAnnotation)
    }
    
    def pickleAnnotation(ann: Annotation) = {
      writeByte(ANNOTATION)
      withLength { pickleType(ann.symbol.typeRef); pickleTree(ann.tree) }
    }

    trees.foreach(pickleTree)
    assert(forwardSymRefs.isEmpty, i"unresolved symbols: ${forwardSymRefs.keySet.toList}%, %")
    compactify()
  }  
}
