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
      
      case _ => ???
    }
    case FunctionDecl(t, dtor, body) => ???
    case StructDecl(id, fields) => ???
  }
}