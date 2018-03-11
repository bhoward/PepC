package edu.depauw.pepc

sealed trait Expr
final case class BinOpExpr(e1: Expr, op: String, e2: Expr) extends Expr
final case class UnOpExpr(op: String, e: Expr) extends Expr
final case class IdExpr(id: String) extends Expr
final case class StrExpr(s: String) extends Expr
final case class IndexExpr(e1: Expr, e2: Expr) extends Expr
final case class CallExpr(e1: Expr, args: Seq[Expr]) extends Expr
final case class FieldExpr(e1: Expr, id: String) extends Expr
final case class PostOp(e: Expr, op: String) extends Expr
final case class CastExpr(t: Type , e: Expr) extends Expr

sealed trait ConstExpr extends Expr
final case class IntExpr(n: Int) extends ConstExpr
final case class CharExpr(c: Char) extends ConstExpr
final case class BoolExpr(b: Boolean) extends ConstExpr
final case class SizeOfExpr(t: Type) extends ConstExpr

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

sealed trait Type
final case object VoidType extends Type
final case object CharType extends Type
final case object IntType extends Type
final case object BoolType extends Type
final case object ConstCharType extends Type
final case object ConstIntType extends Type
final case class StructType(id: String) extends Type
final case class PtrType(t: Type) extends Type

