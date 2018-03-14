package edu.depauw.pepc

class SymbolTable(entries: List[(String, Entry)] = Nil) {
  def add(id: String, entry: Entry): SymbolTable =
    new SymbolTable(entries :+ (id, entry))

  def lookup(id: String): Option[Entry] =
    entries.find(_._1 == id).map(_._2)

  override def toString: String = entries.mkString("\n")
}

object SymbolTable {
  val empty = new SymbolTable

  val init = empty
    .add("true", IntConst(1))
    .add("false", IntConst(0))
  // TODO include scanf, printf, malloc, ...
}

case class Context(globals: SymbolTable, params: SymbolTable, locals: SymbolTable)

sealed trait Entry
final case class CharConst(ch: Char) extends Entry
final case class IntConst(n: Int) extends Entry
final case class StructVar(structId: String) extends Entry
final case class StructPtr(structId: String) extends Entry
final case object CharVar extends Entry
final case object CharPtr extends Entry
final case object IntVar extends Entry
final case object IntPtr extends Entry
final case class CharArray(dim: Int) extends Entry
final case class IntArray(dim: Int) extends Entry
final case class FunDef(t: Type, params: SymbolTable, locals: SymbolTable) extends Entry
final case class StructDef(fields: SymbolTable) extends Entry