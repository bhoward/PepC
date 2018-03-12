package edu.depauw.pepc

class SymbolTable {
  def add(id: String, entry: Entry): SymbolTable = ???
}

object SymbolTable {
  val init = new SymbolTable
}

sealed trait Entry
final case class CharConst(ch: Char) extends Entry
final case class IntConst(n: Int) extends Entry
