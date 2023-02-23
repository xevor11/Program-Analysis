
package hwk;

import java.io.File

/*
1. Imports the java.io.File class.
2. Defines a Scala object named "Main".
3. Defines a main method that takes an array of strings as input (which are not used in this program).
4. Initializes a variable named "ast" with the abstract syntax tree generated from the "rd.js" file using the GenerateAST method.
5. Calls the prep method on the ast variable.
6. Calls the buildGraph method on the ast variable.
7. Initializes a variable named "a" with an Analysis object that takes the ast variable as input.
8. Calls the worklist method on the a variable.
9. Iterates over the nodes in the a variable and prints out their entry and exit sets, sorted by statement id.
10. Prints a newline character.
11. Prints the analysis object's graph representation in the DOT graph language format.
*/
object Main {
  def main(args: Array[String]) { 
    val ast = GenerateAST(new File("test/rd.js"))
    ast.prep 
    
    ast.buildGraph
    
    val a = Analysis(ast) 
    
    a.worklist
    
    for(n <- a.nodes.sortBy(x => x.stmt.id)) {
      println(f"${n.stmt.id}%-4d ${n.entry.toList.sortBy(x=>x).mkString(" ")}%-40s ${n.exit.toList.sortBy(x=>x).mkString(" ")}")
    }
    
    println
    
    print(a.toDotGraph)
  }
}
