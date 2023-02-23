
package hwk;


/*
Implementation of a compiler
  Defines a sealed abstract class Statement representing an abstract syntax tree node for a JavaScript statement
Provides implementations of various methods for working with these nodes:
  i) prep method: performs preprocessing on the syntax tree
      a) Sets unique IDs and heights for each node
      b) Recursively prepares any child nodes
  2) toString method: generates a string representation of the syntax tree for debugging or output
      a) entry and exit methods: determine the entry and exit points of a statement
  3) Defines various case classes that inherit from Statement, such as:
      i) IfStmt
      ii) WhileStmt
      iii) FunctionDecl
      iv) ReturnStmt
  4) Each class represents a different type of JavaScript statement
  5) Classes have their own specific fields and methods that allow them to be manipulated by the compiler.
  */

/*
  The method performs the following actions for each node in the syntax tree:

  1. Sets a unique ID for the node (setid method)
  2. Sets a height for the node
  3. Recursively prepares any child nodes

The prep method uses pattern matching to handle different types of nodes in the syntax tree, and applies the appropriate actions for each type of node:
  i) For Script, BlockStmt, and VarDeclListStmt nodes, it adds heights to the list of statements or declarations and recursively prepares each statement or declaration.
  ii) For FunctionDecl nodes, it sets the height of the function body and prepares the function body.
  iii) For IfStmt nodes, it adds heights for the then and else parts, and recursively prepares each part.
  iv) For SwitchStmt nodes, it adds heights for each case and the default case, and recursively prepares each case.
  v) For CaseStmt, DoWhileStmt, WhileStmt, ForStmt, ForInStmt, and LabeledStmt nodes, it adds height to the body and recursively prepares the body.
  vi) For all other types of nodes, it does nothing.*/
sealed abstract class Statement extends AbstractSyntaxTree {
  // add label to statements and add height
  def prep {
    this.setid
    this match {
    case Script(stmts) => { addHeights(stmts); stmts.foreach(s => s.prep) }
    case BlockStmt(stmts) => { addHeights(stmts); stmts.foreach(s => s.prep) }
    case VarDeclListStmt(decls) => { addHeights(decls); decls.foreach(s => s.prep) }
    case FunctionDecl(_, fun) => { fun.height = height; addHeight(fun.asInstanceOf[FunctionExpr].body) }
    case IfStmt(_, thenPart, elsePart) => { addHeights(List(thenPart, elsePart)); thenPart.prep; elsePart.prep } 
    case SwitchStmt(_, cases, defaultCase) => { 
      val d =  cases ++ (defaultCase match {case Some(c) => {c.default=true; List(c)} case None => List()})
      addHeights(d)
      d.foreach(c => c.prep)
    }
    case CaseStmt(_, body) => { addHeight(body); body.prep }
    case DoWhileStmt(_, body) => { addHeight(body); body.prep }
    case WhileStmt(_, body) => { addHeight(body); body.prep }
    case ForStmt(_, _, _, body) => {  addHeight(body); body.prep }
    case ForInStmt(_, _, body) => { addHeight(body); body.prep }
    case LabeledStmt(_, body) => { addHeight(body); body.prep }
    case _ => 
    }
  }

/*
  This is an implementation of the toString method for a sealed abstract class Statement in a javascript compiler
  -> The method returns a string representation of the statement object, with formatting to make it more human -readable.
     The specific formatting depends on the type of statement represented
  -> Here is a summary of what each case in the match block does:
  i) Script: Calls the toString method on the list of statements in the script
  ii) BlockStmt: Adds an opening brace and newline character, calls the toString method on the list of statements in the block, and adds a closing brace.
  iii) VarDeclListStmt: Calls the toString method on the list of variable declarations
  iv) EmptyStmt: Returns an empty string
  v) ExprStmt: Returns a string representation of the expression statement
  vi) VarDeclStmt: Returns a string representation of the variable declaration statement, including the variable name and an optional assignment
  vii) FunctionDecl: Calls the toString method on the function declaration
  viii) ReturnStmt: Returns a string representation of the return statement.
  ix) IfStmt: Returns a string representation of the if statement, including the condition, then clause, and an optional else clause.
  x) SwitchStmt: Returns a string representation of the switch statement, including the condition and a list of cases
  xi) CaseStmt: Returns a string representation of a case statement, including the case expression and the case body.
  xii) BreakStmt: Returns a string representation of the break statement, including an optional label
  xiii) ContinueStmt: Returns a string representation of the continue statement, including an optional label
  xiv) DoWhileStmt: Returns a string representation of the do - while statement, including the loop condition and loop body.
  xv) WhileStmt: Returns a string representation of the while statement, including the loop condition and loop body.
  xvi) ForStmt: Returns a string representation of the for statement, including the initialization, condition, and increment clauses, as well as the loop body
  xvii) ForInStmt: Returns a string representation of the for -in statement, including the initialization and expression, as well as the loop body
  xviii) LabeledStmt: Returns a string representation of a labeled statement, including the label and the labeled statement body
  xix) _: Returns an empty string for any statement not covered by the other cases

  .*/
  override def toString = this match {
    case Script(stmts) => toString(stmts)
    case BlockStmt(stmts) => space + "{\n" + toString(stmts) + space + "}"
    case VarDeclListStmt(decls) => toString(decls)
    case EmptyStmt() => ""
    case ExprStmt(expr) => space + expr.toString
    case VarDeclStmt(name, expr) => { 
      val e = expr match { case EmptyExpr() => "" case _ => " = " + expr }
      space + "var " + name + e
    }
    case FunctionDecl(name, fun) =>  fun.toString
    case ReturnStmt(expr) => space + "return " + expr
    case IfStmt(cond, thenPart, elsePart) => { 
      val e = elsePart match { case EmptyStmt() => "" case _ => " else\n" + elsePart }
      space + "if (" + cond + ") " + "\n" + thenPart + e
    }
    case SwitchStmt(cond, cases, defaultCase) => {      
      val d =  cases ++ (defaultCase match {case Some(c) => List(c) case None => List()}) 
      space + "switch (" + cond + ") {\n" + d.foldRight("")((s, c) => s + "\n" + c) + space + "}"
    }
    case c@CaseStmt(expr, body) => { 
      c.default match {
        case true => space + "default :\n" + body
        case false => space + "case " + expr + " :\n" + body
      }
    }
    case BreakStmt(breakLabel) => space + "break " + breakLabel
    case ContinueStmt(continueLabel) => space + "continue " + continueLabel
    case DoWhileStmt(cond, body) => space + "do\n" + body + "while (" + cond + ")\n" 
    case WhileStmt(cond, body) =>  space + "while (" + cond + ")\n" + body  
    case ForStmt(init, cond, increment, body) => { 
      val c = cond match { case Some(x)=>x.toString case None => "" }
      val i = increment match { case Some(x)=>x.toString case None => "" }
      space + "for (" + init + "; " + c + "; " + i + ")\n" + body
    }
    case ForInStmt(init, expr, body) => space + "for (" + init + " in " + expr + ")" + body
    case LabeledStmt(label, body) => label.foldRight("")((e,c) => space + e + ":\n" + c) + body 
    case _ => ""
  }

  /*This method takes a list of statements as input and returns a string representation of the statements.
    -> It iterates over the list of statements using the foldRight method to build the resulting string as it goes.
    -> For certain types of statements(WhileStmt, DoWhileStmt, ForStmt, ForInStmt, FunctionDecl, IfStmt, and SwitchStmt), it adds a newline character after the statement in the resulting string
    -> For all other types of statements, it appends the semicolon character and a newline character to the resulting string.
    -> Finally, the method returns the resulting string
  .*/
  def toString(stmts: List[Statement]) = stmts.foldRight("")((s, c) =>
    (s match {
      case WhileStmt(_,_) => s+"\n"
      case DoWhileStmt(_,_) => s+"\n"
      case ForStmt(_,_,_,_) => s+"\n"
      case ForInStmt(_,_,_) => s+"\n"
      case FunctionDecl(_,_) => s+"\n"
      case IfStmt(_,_,_) => s+"\n"
      case SwitchStmt(_,_,_) => s+"\n"
      case EmptyStmt() => ""
      case _ =>  s + ";\n"
    }) + c
  )
  
  def entry: Statement = this match {
    case Script(stmts) => stmts.head.entry       // assume script or block is not empty
    case BlockStmt(stmts) => stmts.head.entry
    case VarDeclListStmt(decls) => decls.head.entry
    case DoWhileStmt(_, body) => body.entry 
    case LabeledStmt(_, stmt) => stmt.entry
    case _ => this
  }

 /* This is a method that returns the entry point of a statement. It checks the type of the statement with a pattern matching expression

  If the statement is a Script, BlockStmt, or VarDeclListStmt, it returns the entry point of the first statement in the list of statements contained within the statement
  If the statement is a DoWhileStmt or LabeledStmt, it returns the entry point of the statement in its body.
  For all other types of statements, it simply returns the statement itself as the entry point
  .*/
  def exit: List[Statement] = this match {
    case Script(stmts) => stmts.last.exit       // assume script or block is not empty
    case BlockStmt(stmts) => stmts.last.exit
    case VarDeclListStmt(decls) => decls.last.exit 
    case IfStmt(_, thenPart, elsePart) => thenPart.exit ::: (elsePart match { case EmptyStmt() => List(this) case _ => elsePart.exit })
    case SwitchStmt(_, cases, defaultCase) => {
       val d = defaultCase match {case None => cases case Some(x) => cases++List(x)}
       d.last.exit
    }
    case CaseStmt(_,body) => body.exit 
    case LabeledStmt(_, stmt) => stmt.exit
    case _ => List(this)
  }

  /*  This code initializes two empty lists of type List[Statement] named succ and pred
    -> It defines a method addSucc that takes a Statement object as input.
    -> When addSucc is called on a statement, it adds the statement to the succ list of the current statement (i.e., the statement on which addSucc is being called).
    -> It also adds the current statement to the pred list of the input statement
    -> This code maintains the successor and predecessor relationships between statements in a program.
    */
  var succ : List[Statement] = List()
  var pred : List[Statement] = List()
  
  def addSucc(s: Statement) { succ = s :: succ; s.pred = this :: s.pred }
  
  def buildGraph(stmts: List[Statement]) {
    stmts match {
      case Nil => 
      case List(s) => s.buildGraph 
      case s1::s2::r => {
        s1.buildGraph
        buildGraph(s2::r)
        val ex = s1.exit
        val et = s2.entry
        for(e <- ex) {
          e.addSucc(et)
        }
      }
    }
  }

  def buildGraph: Unit = this match {
    case Script(stmts) => buildGraph(stmts)  
    case BlockStmt(stmts) => buildGraph(stmts)
    case VarDeclListStmt(decls) => buildGraph(decls)
    case IfStmt(_, thenPart, elsePart) => {
      thenPart.buildGraph
      this.addSucc(thenPart.entry)
      elsePart match {
        case EmptyStmt() => 
        case _ => { 
          elsePart.buildGraph
          this.addSucc(elsePart.entry)
        }
      }
    }
    case SwitchStmt(_, cases, defaultCase) => {
      val d = defaultCase match {case None => cases case Some(x) => cases++List(x)}
      buildGraph(d)
      for(c <- d) {
        addSucc(c.entry) 
      }
    }
    case DoWhileStmt(_, body) => {
      body.buildGraph
      this.addSucc(body.entry)
      for(e <-body.exit) {
        e.addSucc(this)
      }
    }
    case WhileStmt(_, body) => {
      body.buildGraph
      this.addSucc(body.entry)
      for(e <- body.exit) {
        e.addSucc(this)
      }
    }
    case _ =>
  }
  
  def dotStr : String = "\"" + this.id + " : " + (this match {
    case Script(stmts) => stmts.head.dotStr
    case BlockStmt(stmts) => stmts.head.dotStr
    case VarDeclListStmt(decls) => decls.head.dotStr 
    case IfStmt(cond, _, _) => cond.toString
    case WhileStmt(cond, _) => cond.toString
    case DoWhileStmt(cond, _) => cond.toString
    case FunctionDecl(name, _) => "function " + name 
    case _ => this.toString.trim 
  }) + "\""
  
  def toDot : List[String] = this match {
    case Script(stmts) => toDot(stmts)
    case BlockStmt(stmts) => toDot(stmts)
    case VarDeclListStmt(decls) => toDot(decls)
    case IfStmt(cond, thenPart, elsePart) => thenPart.toDot ++ elsePart.toDot
    case WhileStmt(cond, body) => List(toSubgraph(body.toDot, this.id))
    case DoWhileStmt(cond, body) => List(toSubgraph(body.toDot, this.id)) 
    case _ => Nil
  }
  
  def toDot (stmts: List[Statement]) : List[String] = stmts.flatMap(s => s.toDot ++ s.succ.map(e => s.id + " -> " + e.id))  
  
  def toSubgraph (edges: List[String], id: Long) = "subgraph cluster_" + id + 
                                         " {\n" + (edges.reduceLeft((c,e) => c + "\n" + e)) + "\n}" 
}

case class Script(stmts : List[Statement]) extends  Statement
case class BlockStmt(stmts : List[Statement]) extends Statement
case class VarDeclListStmt(decls : List[Statement]) extends Statement
case class EmptyStmt() extends Statement
case class ExprStmt(expr : Expression) extends Statement()
case class VarDeclStmt(name : IntroduceVar, expr : Expression) extends Statement
case class FunctionDecl(name : IntroduceVar, fun : Expression) extends Statement
case class ReturnStmt(expr : Expression) extends Statement
case class IfStmt(cond : Expression, thenPart : Statement, elsePart : Statement) extends Statement
case class SwitchStmt(cond : Expression, cases : List[CaseStmt], defaultCase : Option[CaseStmt]) extends Statement
case class CaseStmt(expr : Expression, body : Statement) extends Statement { var default = false }
case class BreakStmt(breakLabel : String) extends Statement
case class ContinueStmt(continueLabel : String) extends Statement
case class DoWhileStmt(cond : Expression, body : Statement) extends Statement
case class WhileStmt(cond : Expression, body : Statement) extends Statement
case class ForStmt(init : ForInit, cond : Option[Expression], increment : Option[Expression], body : Statement) extends Statement
case class ForInStmt(init : ForInInit, expr : Expression, body : Statement) extends Statement
case class LabeledStmt(label : List[String], stmt : Statement) extends Statement
case class TryStmt(body : Statement, catchClause : List[CatchStmt], finalCatch : Option[Statement]) extends Statement
case class CatchStmt(name : IntroduceVar, body : Statement) extends Statement
case class ThrowStmt(expr : Expression) extends Statement
