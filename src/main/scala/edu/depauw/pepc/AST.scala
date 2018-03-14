package edu.depauw.pepc

sealed trait Expr {
  def value: Int = sys.error("Unable to evaluate expression at compile time")
}
final case class BinOpExpr(e1: Expr, op: String, e2: Expr) extends Expr {
  override def value: Int = {
    val v1 = e1.value
    val v2 = e2.value
    op match {
      case "+" => v1 + v2
      case "-" => v1 - v2
      case "*" => v1 * v2
      case "/" => v1 / v2
      case "%" => v1 % v2
      case "&" => v1 & v2
      case "|" => v1 | v2
      case "^" => v1 ^ v2
      case "<<" => v1 << v2
      case ">>" => v1 >> v2
      case "==" => if (v1 == v2) 1 else 0
      case "!=" => if (v1 != v2) 1 else 0
      case "<=" => if (v1 <= v2) 1 else 0
      case ">=" => if (v1 >= v2) 1 else 0
      case "<" => if (v1 < v2) 1 else 0
      case ">" => if (v1 > v2) 1 else 0
      case "&&" => if (v1 != 0 && v2 != 0) 1 else 0
      case "||" => if (v1 != 0 || v2 != 0) 1 else 0
      case _ => sys.error(s"Unable to evaluate operator $op at compile time")
    }
  }
}
final case class UnOpExpr(op: String, e: Expr) extends Expr {
  override def value: Int = {
    val v = e.value
    op match {
      case "+" => +v
      case "-" => -v
      case "~" => ~v
      case "!" => if (v == 0) 1 else 0
      case _ => sys.error(s"Unable to evaluate unary $op at compile time")
    }
  }
}
final case class PostOp(e: Expr, op: String) extends Expr
final case class IdExpr(id: String) extends Expr // TODO look for a constant value in symbol table?
final case class StrExpr(s: String) extends Expr
final case class IndexExpr(e1: Expr, e2: Expr) extends Expr
final case class CallExpr(e1: Expr, args: Seq[Expr]) extends Expr
final case class FieldExpr(e1: Expr, id: String) extends Expr
final case class CastExpr(t: Type , e: Expr) extends Expr

sealed trait ConstExpr extends Expr
final case class IntExpr(n: Int) extends ConstExpr {
  override def value = n
}
final case class CharExpr(c: Char) extends ConstExpr {
  override def value = c.toInt
}
final case class BoolExpr(b: Boolean) extends ConstExpr {
  override def value = if (b) 1 else 0
}
final case class SizeOfExpr(t: Type) extends ConstExpr {
  override def value = t.size
}

sealed trait Stmt
final case class CompoundStmt(decls: Seq[Decl], stmts: Seq[Stmt]) extends Stmt
final case class LabelStmt(label: String, stmt: Stmt) extends Stmt
final case class CaseStmt(value: ConstExpr, stmt: Stmt) extends Stmt
final case class DefaultStmt(stmt: Stmt) extends Stmt
final case class ExprStmt(e: Expr) extends Stmt
final case class IfStmt(test: Expr, yes: Stmt, no: Stmt) extends Stmt
final case class SwitchStmt(e: Expr, s: Stmt) extends Stmt
final case class WhileStmt(test: Expr, body: Stmt) extends Stmt
final case class DoStmt(body: Stmt, test: Expr) extends Stmt
final case class ForStmt(init: Stmt, test: Expr, update: Stmt, body: Stmt) extends Stmt
final case class GotoStmt(label: String) extends Stmt
final case class ReturnStmt(e: Expr) extends Stmt
final case object ContinueStmt extends Stmt
final case object BreakStmt extends Stmt
final case object EmptyStmt extends Stmt

sealed trait Decl
final case class VarDecl(t: Type, dtor: Dtor) extends Decl
final case class FunctionDecl(t: Type, dtor: FunDtor, body: Stmt) extends Decl
final case class StructDecl(id: String, fields: Seq[Decl]) extends Decl

sealed trait Dtor
final case class InitDtor(dtor: Dtor, init: Expr) extends Dtor
final case class VarDtor(id: String) extends Dtor
final case class ArrayDtor(id: String, dim: Int) extends Dtor
final case class PtrDtor(id: String) extends Dtor

final case class FunDtor(id: String, params: Seq[Decl])

sealed trait Type {
  def size: Int
}
final case object VoidType extends Type {
  def size: Int = 0
}
final case object CharType extends Type {
  def size: Int = 1
}
final case object IntType extends Type {
  def size: Int = 2
}
final case object BoolType extends Type {
  def size: Int = 2
}
final case class ConstType(t: Type) extends Type {
  def size: Int = t.size
}
final case class StructType(id: String) extends Type {
  def size: Int = ??? // TODO value and size need to be passed a symbol table
}
final case class PtrType(t: Type) extends Type {
  def size: Int = 2
}

