package edu.depauw.pepc

object CodeGen {
  sealed trait StackCode
  final case class PushWordVar(id: String) extends StackCode
  final case class PushWordPtr(id: String) extends StackCode
  final case class PushByteVar(id: String) extends StackCode
  final case class PushWordConst(n: Int) extends StackCode
  final case class PushByteConst(n: Int) extends StackCode
  final case object Add extends StackCode
  final case object Sub extends StackCode
  final case object Mul extends StackCode
  final case object Div extends StackCode
  final case object Mod extends StackCode
  final case object And extends StackCode
  final case object Or extends StackCode
  final case object Xor extends StackCode
  final case object LShift extends StackCode
  final case object RShift extends StackCode
  final case object Neg extends StackCode
  final case object Not extends StackCode

  def genStackCode(expr: Expr, context: Context): List[StackCode] = expr match {
    case BinOpExpr(e1, op, e2) =>
      val e1Code = genStackCode(e1, context)
      val e2Code = genStackCode(e2, context)
      op match {
        case "+"  => e1Code ::: e2Code ::: List(Add)
        case "-"  => e1Code ::: e2Code ::: List(Sub)
        case "*"  => e1Code ::: e2Code ::: List(Mul)
        case "/"  => e1Code ::: e2Code ::: List(Div)
        case "%"  => e1Code ::: e2Code ::: List(Mod)
        case "&"  => e1Code ::: e2Code ::: List(And)
        case "|"  => e1Code ::: e2Code ::: List(Or)
        case "^"  => e1Code ::: e2Code ::: List(Xor)
        case "<<" => e1Code ::: e2Code ::: List(LShift)
        case ">>" => e1Code ::: e2Code ::: List(RShift)
        case _    => ??? // TODO
      }

    case UnOpExpr(op, e) =>
      val eCode = genStackCode(e, context)
      op match {
        case "+" => eCode
        case "-" => eCode ::: List(Neg)
        case "~" => eCode ::: List(Not)
        case _   => ??? // TODO
      }

    case PostOp(e, op) =>
      val eCode = genStackCode(e, context)
      op match {
        case _ => ??? // TODO
      }

    case IdExpr(id) =>
      val localEntry = context.locals.lookup(id).orElse(context.params.lookup(id))
      localEntry match {
        case Some(entry) =>
          // it's a local id
          ???
        case None =>
          // it's a global id, or an error
          val entry = context.globals.lookup(id).getOrElse(sys.error(s"Undefined symbol $id"))
          ???
      }
      
    case _ => ???
  }
}