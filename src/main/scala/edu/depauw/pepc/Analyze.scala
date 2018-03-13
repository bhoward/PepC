package edu.depauw.pepc

object Analyze {
  def collectProgram(decls: Seq[Decl]): SymbolTable =
    decls.foldLeft(SymbolTable.init)(collectDecl)

  def collectDecl(table: SymbolTable, decl: Decl): SymbolTable = decl match {
    case VarDecl(t, dtor) => (t, dtor) match {
      case (ConstType(CharType), InitDtor(VarDtor(id), e)) =>
        table.add(id, CharConst(e.value.toChar))
      case (ConstType(IntType), InitDtor(VarDtor(id), e)) =>
        table.add(id, IntConst(e.value))
      case (ConstType(BoolType), InitDtor(VarDtor(id), e)) =>
        table.add(id, IntConst(e.value))
      case (ConstType(_), InitDtor(_, _)) =>
        sys.error("Constant declaration of unsupported type")
      case (ConstType(_), _) =>
        sys.error("Constant declaration without initializer")
      case (_, InitDtor(_, _)) =>
        sys.error("Initializer of non-constant unsupported")
      case (StructType(sid), VarDtor(vid)) =>
        table.add(vid, StructVar(sid))
      case (StructType(sid), PtrDtor(pid)) =>
        table.add(pid, StructPtr(sid))
      case (PtrType(StructType(sid)), VarDtor(vid)) =>
        table.add(vid, StructPtr(sid))
      case (CharType, VarDtor(vid)) =>
        table.add(vid, CharVar)
      case (IntType, VarDtor(vid)) =>
        table.add(vid, IntVar)
      case (BoolType, VarDtor(vid)) =>
        table.add(vid, IntVar)
      case (CharType, PtrDtor(vid)) =>
        table.add(vid, CharPtr)
      case (IntType, PtrDtor(vid)) =>
        table.add(vid, IntPtr)
      case (BoolType, PtrDtor(vid)) =>
        table.add(vid, IntPtr)
      case (PtrType(CharType), VarDtor(vid)) =>
        table.add(vid, CharPtr)
      case (PtrType(IntType), VarDtor(vid)) =>
        table.add(vid, IntPtr)
      case (PtrType(BoolType), VarDtor(vid)) =>
        table.add(vid, IntPtr)
      case (CharType, ArrayDtor(aid, dim)) =>
        table.add(aid, CharArray(dim))
      case (IntType, ArrayDtor(aid, dim)) =>
        table.add(aid, IntArray(dim))
      case (BoolType, ArrayDtor(aid, dim)) =>
        table.add(aid, IntArray(dim))
      // TODO arrays of pointers?
      case _ =>
        sys.error("Unsupported declaration")
    }

    case FunctionDecl(t, dtor, body) => (t, dtor) match {
      case (ConstType(_), _) =>
        sys.error("Function return may not be a const")
      case (StructType(_), _) =>
        sys.error("Function return may not be a struct")
      case (_, FunDtor(id, params)) =>
        val paramTable = collectParams(params)
        val localTable = collectStmt(body)
        table.add(id, FunDef(t, paramTable, localTable))
    }

    case StructDecl(id, fields) => {
      val fieldTable = collectFields(fields)
      table.add(id, StructDef(fieldTable))
    }
  }

  def collectParams(params: Seq[Decl]): SymbolTable =
    params.foldLeft(SymbolTable.empty)(collectDecl)

  def collectStmt(stmt: Stmt): SymbolTable = stmt match {
    case CompoundStmt(decls, stmts) =>
      decls.foldLeft(SymbolTable.empty)(collectDecl)
    case _ =>
      // ignore embedded declarations -- only allowed at top of function body
      SymbolTable.empty
  }

  def collectFields(fields: Seq[Decl]): SymbolTable =
    fields.foldLeft(SymbolTable.empty)(collectDecl)
}