package edu.depauw.pepc

// TODO add comments, #include

object Grammar {
  val White = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._

    val ws = P { CharIn(" \t\n\r") }

    val comment = P {
      "/*" ~ ("x").rep ~ "*/" // TODO
    }

    NoTrace((ws | comment).rep)
  }

  import fastparse.noApi._
  import White._
  import Lexical._

  val translationUnit = P {
    externalDeclaration.rep ~ End
  }

  val externalDeclaration = P {
    functionDefinition | declaration
  }

  val functionDefinition = P {
    typeSpecifier ~ functionDeclarator ~ compoundStatement
  }

  val declaration = P {
    typeSpecifier ~ initDeclarator.rep(1) ~ ";"
  }

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

  val assignmentExpression = P {
    conditionalExpression.rep(1, assignmentOperator) // TODO should be unaryExpression on the lhs
  }

  val assignmentOperator = P {
    "=" | "+=" | "-=" // TODO
  }

  val unaryExpression = P {
    unaryOperator.rep ~ postfixExpression
  }

  val unaryOperator = P {
    "&" | "*" | "+" | "-" | "~" | "!" | "++" | "--"
  }

  val postfixExpression = P {
    primaryExpression ~ (
      ("[" ~/ expression ~ "]") |
      ("(" ~/ expression.rep(0, ",") ~ ")") |
      ("." ~/ identifier) |
      ("->" ~/ identifier) |
      "++" |
      "--").rep
  }

  val primaryExpression = P {
    identifier | constant | string | ("(" ~/ expression ~ ")")
  }

  val expression: P[Unit] = P {
    assignmentExpression.map(_ => ())
  }

  val conditionalExpression = P {
    logicalOrExpression
  }

  val logicalOrExpression = P {
    logicalAndExpression.rep(1, "||")
  }

  val logicalAndExpression = P {
    inclusiveOrExpression.rep(1, "&&")
  }

  val inclusiveOrExpression = P {
    exclusiveOrExpression.rep(1, "|")
  }

  val exclusiveOrExpression = P {
    andExpression.rep(1, "^")
  }

  val andExpression = P {
    relationalExpression.rep(1, "&")
  }

  val relationalExpression = P {
    shiftExpression ~ (("==" | "!=" | "<=" | ">=" | "<" | ">") ~ shiftExpression).?
  }

  val shiftExpression = P {
    additiveExpression.rep(1, "<<" | ">>")
  }

  val additiveExpression = P {
    multiplicativeExpression.rep(1, "+" | "-")
  }

  val multiplicativeExpression = P {
    unaryExpression.rep(1, "*" | "/" | "%")
  }

  val parameterList = P {
    parameterDeclaration.rep(0, ",")
  }

  val parameterDeclaration = P {
    typeSpecifier ~ "*".rep ~ identifier
  }

  val compoundStatement = P {
    "{" ~ declaration.rep ~ statement.rep ~ "}"
  }

  val constant = P {
    integerConstant | characterConstant
  }

  val statement: P[Unit] = P {
    (labeledStatement |
      jumpStatement |
      compoundStatement |
      selectionStatement |
      iterationStatement |
      expressionStatement).map(_ => ())
  }

  val labeledStatement = P {
    (identifier ~ ":" ~/ statement) |
      ("case" ~/ constant ~ ":" ~ statement) |
      ("default" ~/ ":" ~ statement)
  }

  val expressionStatement = P {
    expression.? ~ ";"
  }

  val selectionStatement = P {
    ("if" ~/ "(" ~ expression ~ ")" ~ statement ~ ("else" ~ statement).?) |
      ("switch" ~/ "(" ~ expression ~ ")" ~ statement)
  }

  val iterationStatement = P {
    ("while" ~/ "(" ~ expression ~ ")" ~ statement) |
      ("do" ~/ statement ~ "while" ~ "(" ~ expression ~ ")" ~ ";") |
      ("for" ~/ "(" ~ expression.? ~ ";" ~ expression.? ~ ";" ~ expression.? ~ ")" ~ statement)
  }

  val jumpStatement = P {
    ("goto" ~/ identifier ~ ";") |
      ("continue" ~/ ";") |
      ("break" ~/ ";") |
      ("return" ~/ expression ~ ";")
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