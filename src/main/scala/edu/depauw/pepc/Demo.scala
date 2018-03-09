package edu.depauw.pepc

object Demo extends App {
  val src =
    """int main() {
      |   printf("Hello, world!\n");
      |   return 0;
      |}
    """.stripMargin
  
  val result = Grammar.functionDefinition.parse(src)
  
  println(result)
}