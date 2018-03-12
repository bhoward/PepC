package edu.depauw.pepc

object CodeGen {
  sealed trait StackCode
  final case class PushVar(id: String) extends StackCode
  
}