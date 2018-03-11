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
  
  import scala.io.Source
  
  val examples =
    List("519", "522", "527", "604", "606", "608", "610", "612",
      "614", "618", "621", "623", "625", "627", "629", "632",
      "634", "636", "638", "640", "642", "644", "646", "648")
  for (example <- examples) {
    val file = s"examples/fig0$example.c"
    println(file)
    val source = Source.fromFile(file).mkString
    println(Grammar.translationUnit.parse(source))
  }
}