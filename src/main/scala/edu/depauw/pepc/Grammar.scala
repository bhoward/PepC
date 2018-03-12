package edu.depauw.pepc

object Grammar {
  val White = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._

    val ws = P { CharIn(" \t\f\n\r") }

    val comment = P {
      ("/*" ~/ (CharsWhile(_ != '*') | ("*" ~ !"/")).rep ~ "*/") |
        ("//" ~/ CharsWhile(_ != '\n').? ~ "\n")
    }

    NoTrace((ws | comment).rep)
  }

  import fastparse.noApi._
  import White._
  import Lexical._

  val translationUnit: P[Seq[Decl]] = P {
    Start ~ include.rep ~ externalDeclaration.rep.map(_.flatten) ~ End
  }

  // These are just here to be ignored -- there will be no preprocessor...
  val include = P {
    "#include" ~ "<" ~ CharsWhile(_ != '>') ~ ">"
  }

  val externalDeclaration: P[Seq[Decl]] = P {
    functionDefinition.map(d => Seq(d)) |
      structDefinition.map(d => Seq(d)) |
      declaration
  }

  val functionDefinition: P[Decl] = P {
    (typeSpecifier ~ functionDeclarator ~ compoundStatement).map {
      case (t, dtor, body) => FunctionDecl(t, dtor, body)
    }
  }

  val declaration: P[Seq[Decl]] = P {
    (typeSpecifier ~ initDeclarator.rep(1, ",") ~ ";").map {
      case (t, dtors) => dtors.map(dtor => VarDecl(t, dtor))
    }
  }

  val typeSpecifier: P[Type] = P {
    "void".!.map(_ => VoidType) |
      "char".!.map(_ => CharType) |
      "int".!.map(_ => IntType) |
      "bool".!.map(_ => BoolType) |
      ("const" ~ "char").map(_ => ConstType(CharType)) |
      ("const" ~ "int").map(_ => ConstType(IntType)) |
      ("struct" ~ identifier).map(StructType)
  }

  val structDefinition: P[Decl] = P {
    ("struct" ~ identifier ~ "{" ~ structDeclaration.rep(1) ~ "}" ~ ";").map {
      case (id, fields) => StructDecl(id, fields.flatten)
    }
  }

  val structDeclaration: P[Seq[Decl]] = P {
    (typeSpecifier ~ declarator.rep ~ ";").map {
      case (t, dtors) => dtors.map(dtor => VarDecl(t, dtor))
    }
  }

  val functionDeclarator: P[FunDtor] = P {
    (identifier ~ "(" ~/ parameterList ~ ")").map {
      case (id, params) => FunDtor(id, params)
    }
  }

  val parameterList: P[Seq[Decl]] = P {
    parameterDeclaration.rep(0, ",")
  }

  val parameterDeclaration: P[Decl] = P {
    (typeSpecifier ~ "*".!.? ~ identifier ~ ("[" ~ "]").!.?).map {
      case (t, None, id, None)    => VarDecl(t, VarDtor(id))
      case (t, None, id, Some(_)) => VarDecl(t, PtrDtor(id))
      case (t, Some(_), id, _)    => VarDecl(t, PtrDtor(id)) // don't handle arrays of pointers...
    }
  }

  val initDeclarator: P[Dtor] = P {
    (declarator ~ ("=" ~/ assignmentExpression).?).map {
      case (dtor, None)    => dtor
      case (dtor, Some(e)) => InitDtor(dtor, e)
    }
  }

  val declarator: P[Dtor] = P {
    ("*" ~ identifier).map(PtrDtor) |
      (identifier ~ ("[" ~/ integerConstant ~ "]").?).map {
        case (id, None)      => VarDtor(id)
        case (id, Some(dim)) => ArrayDtor(id, dim)
      }
  }

  val assignmentExpression: P[Expr] = P {
    (conditionalExpression ~ (assignmentOperator ~/ assignmentExpression).?).map {
      case (e, None)            => e
      case (e1, Some((op, e2))) => BinOpExpr(e1, op, e2)
    }
  }

  val assignmentOperator: P[String] = P {
    ("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=").!
  }

  val unaryExpression: P[Expr] = P {
    (unaryOperator ~ unaryExpression).map { case (op, e) => UnOpExpr(op, e) } |
      (castOperator ~ unaryExpression).map { case (t, e) => CastExpr(t, e) } |
      ("sizeof" ~ "(" ~ typeSpecifier ~ ")").map { case t => SizeOfExpr(t) } |
      postfixExpression
  }

  val unaryOperator: P[String] = P {
    ("&" | "*" | "+" | "-" | "~" | "!" | "++" | "--").!
  }

  val castOperator: P[Type] = P {
    ("(" ~ typeSpecifier ~ "*".!.? ~ ")").map {
      case (t, None)    => t
      case (t, Some(_)) => PtrType(t)
    }
  }

  val postfixExpression: P[Expr] = P {
    primaryExpression ~ (
      ("[" ~/ expression ~ "]").map(e2 => (e1: Expr) => IndexExpr(e1, e2)) |
      ("(" ~/ expression.rep(sep = ",") ~ ")").map(args => (e1: Expr) => CallExpr(e1, args)) |
      ("." ~/ identifier).map(id => (e1: Expr) => FieldExpr(e1, id)) |
      ("->" ~/ identifier).map(id => (e1: Expr) => FieldExpr(UnOpExpr("*", e1), id)) |
      ("++" | "--").!.map(op => (e1: Expr) => PostOp(e1, op))).rep
  } map {
    case (pe, mods) => mods.foldLeft(pe) { case (e, mod) => mod(e) }
  }

  val primaryExpression: P[Expr] = P {
    constant |
      identifier.map(id => IdExpr(id)) |
      string.map(s => StrExpr(s)) |
      ("(" ~/ expression ~ ")")
  }

  val expression: P[Expr] = P {
    assignmentExpression
  }

  val conditionalExpression: P[Expr] = P {
    logicalOrExpression // TODO add the ?: operator?
  }

  val logicalOrExpression: P[Expr] = P {
    (logicalAndExpression ~ ("||" ~ logicalAndExpression).rep).map {
      case (e, es) => es.foldLeft(e) { case (e1, e2) => BinOpExpr(e1, "||", e2) }
    }
  }

  val logicalAndExpression: P[Expr] = P {
    (inclusiveOrExpression ~ ("&&" ~ inclusiveOrExpression).rep).map {
      case (e, es) => es.foldLeft(e) { case (e1, e2) => BinOpExpr(e1, "&&", e2) }
    }
  }

  val inclusiveOrExpression: P[Expr] = P {
    (exclusiveOrExpression ~ ("|" ~ exclusiveOrExpression).rep).map {
      case (e, es) => es.foldLeft(e) { case (e1, e2) => BinOpExpr(e1, "|", e2) }
    }
  }

  val exclusiveOrExpression: P[Expr] = P {
    (andExpression ~ ("^" ~ andExpression).rep).map {
      case (e, es) => es.foldLeft(e) { case (e1, e2) => BinOpExpr(e1, "^", e2) }
    }
  }

  val andExpression: P[Expr] = P {
    (relationalExpression ~ ("&" ~ relationalExpression).rep).map {
      case (e, es) => es.foldLeft(e) { case (e1, e2) => BinOpExpr(e1, "&", e2) }
    }
  }

  val relationalExpression: P[Expr] = P {
    (shiftExpression ~ (relationalOperator ~ shiftExpression).rep(max = 1)).map {
      case (e, ops) => ops.foldLeft(e) { case (e1, (op, e2)) => BinOpExpr(e1, op, e2) }
    }
  }

  val relationalOperator: P[String] = P {
    ("==" | "!=" | "<=" | ">=" | "<" | ">").!
  }

  val shiftExpression: P[Expr] = P {
    (additiveExpression ~ (shiftOperator ~ additiveExpression).rep).map {
      case (e, ops) => ops.foldLeft(e) { case (e1, (op, e2)) => BinOpExpr(e1, op, e2) }
    }
  }

  val shiftOperator: P[String] = P {
    ("<<" | ">>").!
  }

  val additiveExpression: P[Expr] = P {
    (multiplicativeExpression ~ (additiveOperator ~ multiplicativeExpression).rep).map {
      case (e, ops) => ops.foldLeft(e) { case (e1, (op, e2)) => BinOpExpr(e1, op, e2) }
    }
  }

  val additiveOperator: P[String] = P {
    ("+" | "-").!
  }

  val multiplicativeExpression: P[Expr] = P {
    (unaryExpression ~ (multiplicativeOperator ~ unaryExpression).rep).map {
      case (e, ops) => ops.foldLeft(e) { case (e1, (op, e2)) => BinOpExpr(e1, op, e2) }
    }
  }

  val multiplicativeOperator: P[String] = P {
    ("*" | "/" | "%").!
  }

  val constant: P[ConstExpr] = P {
    integerConstant.map(n => IntExpr(n)) |
      characterConstant.map(c => CharExpr(c)) |
      "true".!.map(_ => BoolExpr(true)) |
      "false".!.map(_ => BoolExpr(false))
  }

  val compoundStatement: P[Stmt] = P {
    ("{" ~ declaration.rep ~ statement.rep ~ "}").map {
      case (decls, stmts) => CompoundStmt(decls.flatten, stmts)
    }
  }

  val statement: P[Stmt] = P {
    labeledStatement |
      jumpStatement |
      compoundStatement |
      selectionStatement |
      iterationStatement |
      expressionStatement
  }

  val labeledStatement: P[Stmt] = P {
    (identifier ~ ":" ~/ statement).map { case (id, s) => LabelStmt(id, s) } |
      ("case" ~/ constant ~ ":" ~ statement).map { case (v, s) => CaseStmt(v, s) } |
      ("default" ~/ ":" ~ statement).map { case s => DefaultStmt(s) }
  }

  val expressionStatement: P[Stmt] = P {
    (expression.? ~ ";").map {
      case None    => EmptyStmt
      case Some(e) => ExprStmt(e)
    }
  }

  val selectionStatement: P[Stmt] = P {
    ("if" ~/ "(" ~ expression ~ ")" ~ statement ~ ("else" ~ statement).?).map {
      case (e, s, None)      => IfStmt(e, s, EmptyStmt)
      case (e, s1, Some(s2)) => IfStmt(e, s1, s2)
    } |
      ("switch" ~/ "(" ~ expression ~ ")" ~ statement).map {
        case (e, s) => SwitchStmt(e, s)
      }
  }

  val iterationStatement: P[Stmt] = P {
    ("while" ~/ "(" ~ expression ~ ")" ~ statement).map {
      case (e, s) => WhileStmt(e, s)
    } |
      ("do" ~/ statement ~ "while" ~ "(" ~ expression ~ ")" ~ ";").map {
        case (s, e) => DoStmt(s, e)
      } |
      ("for" ~/ "(" ~ expression.? ~ ";" ~ expression.? ~ ";" ~ expression.? ~ ")" ~ statement).map {
        case (initOpt, testOpt, updateOpt, body) =>
          val init = initOpt.map(ExprStmt).getOrElse(EmptyStmt)
          val test = testOpt.getOrElse(IntExpr(1))
          val update = updateOpt.map(ExprStmt).getOrElse(EmptyStmt)
          ForStmt(init, test, update, body)
      }
  }

  val jumpStatement: P[Stmt] = P {
    ("goto" ~/ identifier ~ ";").map(id => GotoStmt(id)) |
      ("continue" ~/ ";").map(_ => ContinueStmt) |
      ("break" ~/ ";").map(_ => BreakStmt) |
      ("return" ~/ expression ~ ";").map(e => ReturnStmt(e))
  }
}

object Lexical {
  import fastparse.all._

  val identifier: P[String] = P {
    (letter ~ (letter | digit).rep).!
  }

  val integerConstant: P[Int] = P {
    ("0x" ~/ hexDigit.rep(1).!.map(Integer.parseInt(_, 16))) |
      ("0" ~/ octalDigit.rep).!.map(Integer.parseInt(_, 8)) |
      digit.rep(1).!.map(_.toInt)
  }

  val string: P[String] = P {
    "\"" ~/ (strChars | escape).rep.! ~ "\""
  }.map(unescape)

  val characterConstant: P[Char] = P {
    "'" ~/ (CharPred(!"'\\\n".contains(_: Char)) | escape).! ~ "'"
  }.map(s => unescape(s).charAt(0))

  val hexEscape = P { "x" ~ hexDigit ~ hexDigit }

  val octalEscape = P {
    octalDigit.rep(min = 1, max = 3)
  }

  val escape = P { "\\" ~ (CharIn("\"'\\abfnrtv") | hexEscape | octalEscape) }

  val strChars = CharsWhile(!"\"\\\n".contains(_: Char))

  val letter = CharIn('A' to 'Z', 'a' to 'z', "_")

  val digit = CharIn('0' to '9')

  val hexDigit = CharIn('0' to '9', 'a' to 'f', 'A' to 'F')

  val octalDigit = CharIn('0' to '7')

  def unescape(s: String): String = {
    var s2 = s.replaceAllLiterally("\\a", "\007").replaceAllLiterally("\\v", "\013")
    while (s2.contains("\\x")) {
      val index = s2.indexOf("\\x")
      val left = s2.substring(0, index)
      val mid = s2.substring(index + 2, index + 4)
      val right = s2.substring(index + 4)
      s2 = left + Integer.parseInt(mid, 16).toChar + right
    }
    StringContext.treatEscapes(s2)
  }
}