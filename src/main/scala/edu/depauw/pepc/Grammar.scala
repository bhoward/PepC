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

  val translationUnit = P {
    Start ~ include.rep ~ externalDeclaration.rep ~ End
  }

  // These are just here to be ignored -- there will be no preprocessor...
  val include = P {
    "#include" ~ "<" ~ CharsWhile(_ != '>') ~ ">"
  }

  val externalDeclaration = P {
    functionDefinition | declaration
  }

  val functionDefinition = P {
    typeSpecifier ~ functionDeclarator ~ compoundStatement
  }

  val declaration: P[Decl] = P {
    typeSpecifier ~ initDeclarator.rep(1) ~ ";"
  }.map(_ => ???)

  val typeSpecifier = P {
    "void" | "char" | "int" // TODO
  }

  val functionDeclarator = P {
    "*".rep ~ identifier ~ "(" ~/ parameterList ~ ")"
  }

  val initDeclarator = P {
    declarator ~ ("=" ~/ assignmentExpression).?
  }

  val declarator = P {
    "*".rep ~ identifier ~ ("[" ~/ integerConstant ~ "]").?
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
      postfixExpression
  }

  val unaryOperator: P[String] = P {
    ("&" | "*" | "+" | "-" | "~" | "!" | "++" | "--").!
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
    identifier.map(id => IdExpr(id)) |
      constant |
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

  val constant: P[Expr] = P {
    integerConstant.map(n => IntExpr(n)) |
      characterConstant.map(c => CharExpr(c))
  }

  val parameterList = P {
    parameterDeclaration.rep(0, ",")
  }

  val parameterDeclaration = P {
    typeSpecifier ~ "*".rep ~ identifier
  }

  val compoundStatement: P[Stmt] = P {
    ("{" ~ declaration.rep ~ statement.rep ~ "}").map {
      case (decls, stmts) => CompoundStmt(decls, stmts)
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
        case (opte1, opte2, opte3, s) => ForStmt(opte2s(opte1), opte2s(opte2), opte2s(opte3), s)
      }
  }

  def opte2s(opte: Option[Expr]): Stmt = opte match {
    case None    => EmptyStmt
    case Some(e) => ExprStmt(e)
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