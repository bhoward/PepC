package edu.depauw.pepc

import fastparse.core.Parsed

object Demo extends App {
  val src =
    """/** demo // hello world */
      |#include <stdio.h>
      |
      |int main(/**/) { //
      |   printf("Hello, world!\n"); // not a /* comment
      |   return 0;/***/
      |}
    """.stripMargin

  println(Grammar.translationUnit.parse(src))
  println(Lexical.string.parse(""""H\145llo\x20World\x2A" """))
  println(Lexical.identifier.parse("H3110_Wørld"))
  println(Lexical.characterConstant.parse("'\\''"))
}