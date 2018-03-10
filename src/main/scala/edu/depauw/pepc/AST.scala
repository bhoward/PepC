package edu.depauw.pepc

sealed trait Expr
final case class BinOpExpr(e1: Expr, op: String, e2: Expr) extends Expr
final case class UnOpExpr(op: String, e: Expr) extends Expr
final case class IdExpr(id: String) extends Expr
final case class IntExpr(n: Int) extends Expr
final case class CharExpr(c: Char) extends Expr
final case class StrExpr(s: String) extends Expr
final case class IndexExpr(e1: Expr, e2: Expr) extends Expr
final case class CallExpr(e1: Expr, args: Seq[Expr]) extends Expr
final case class FieldExpr(e1: Expr, id: String) extends Expr
final case class PostOp(e: Expr, op: String) extends Expr

sealed trait Stmt
final case class CompoundStmt(decls: Seq[Decl], stmts: Seq[Stmt]) extends Stmt
final case class LabelStmt(label: String, stmt: Stmt) extends Stmt
final case class CaseStmt(value: Expr, stmt: Stmt) extends Stmt
final case class DefaultStmt(stmt: Stmt) extends Stmt
final case class ExprStmt(e: Expr) extends Stmt
final case class IfStmt(test: Expr, yes: Stmt, no: Stmt) extends Stmt
final case class SwitchStmt(e: Expr, s: Stmt) extends Stmt
final case class WhileStmt(test: Expr, body: Stmt) extends Stmt
final case class DoStmt(body: Stmt, test: Expr) extends Stmt
final case class ForStmt(init: Stmt, test: Stmt, update: Stmt, body: Stmt) extends Stmt
final case class GotoStmt(label: String) extends Stmt
final case class ReturnStmt(e: Expr) extends Stmt
final case object BreakStmt extends Stmt
final case object ContinueStmt extends Stmt
final case object EmptyStmt extends Stmt

sealed trait Decl