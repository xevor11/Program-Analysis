
package hwk;
 
trait AbstractSyntaxTree {
  var height = 0

  // This method takes an AbstractSyntaxTree object and sets its height to the current object's height plus one.
  def addHeight(ast: AbstractSyntaxTree) = { ast.height = height + 1 }
  // This method takes a list of AbstractSyntaxTree objects and calls addHeight on each of them.
  def addHeights(asts: List[AbstractSyntaxTree]) = asts.foreach(addHeight(_))
  // This method returns a string of spaces with a length equal to the current object's height.
  def space = " " * height
  
  type Label = Long
  var id : Label = -1
  // This method generates a fresh ID and sets it to the object's id variable.
  def setid() { id = AbstractSyntaxTree.freshID }
}

object AbstractSyntaxTree {
  type Label = Long
  private var idSeed: Label = 0
  // The purpose of this method is to generate a new unique identifier every time it is called.
  // The idSeed variable should be initialized somewhere else in the code before this method is called.
  // The method may be used in various places where unique identifiers are needed, such as generating variable names or label names in a compiler or interpreter.
  private def freshID = {
    idSeed += 1
    idSeed
  }
}

// auxiliary definition for variable use/def
// This is a trait that provides a referTo variable that can be used to store information about the variable being accessed.
trait VariableAccess { var referTo : IntroduceVar = null }
// This case class represents the introduction of a new variable and takes a string as its parameter.
case class IntroduceVar(str : String) extends AbstractSyntaxTree { override def toString = str }
 
// auxiliary definition for for-in-loop

// 1. This is an abstract class that represents the initialization of a for-in-loop.

// 2. abstract class ForInInit which extends the AbstractSyntaxTree class. The ForInInit class has two concrete subclasses: ForInVarDecl and ForInLValue.

// 3. The toString method is overridden in the ForInInit class. The toString method is used to return a string representation of the object.

// 4. In this implementation, when the toString method is called on an object of the ForInInit class or its subclasses, a pattern match is used to check which concrete subclass the object belongs to.

//    a. If the object belongs to the ForInVarDecl subclass, then the method returns the string "var " followed by the string representation of the IntroduceVar object contained in the ForInVarDecl object.

//    b. If the object belongs to the ForInLValue subclass, then the method returns the string representation of the LValue object contained in the ForInLValue object.
sealed abstract class ForInInit extends AbstractSyntaxTree {
  override def toString = this match {
    case ForInVarDecl(name) => "var " + name.toString
    case ForInLValue(lVal) => lVal.toString
  }
}

// This case class represents the initialization of a for-in-loop with a variable declaration and takes an IntroduceVar object as its parameter.
case class ForInVarDecl(name : IntroduceVar) extends ForInInit()
// This case class represents the initialization of a for-in-loop with an l-value and takes an LValue object as its parameter.
case class ForInLValue(lVal : LValue) extends ForInInit()

// auxiliary definition for for-loops
// This is an abstract class that represents the initialization of a for-loop.

// Definition an abstract class ForInit which extends AbstractSyntaxTree. This class has four subclasses: NoneInit, VarListInit, VarInit, and ExprInit.
// The toString method of ForInit returns a string representation of the object that it is called on.
// The implementation of toString checks which subclass the object belongs to and returns a string based on that.

// A. NoneInit is an empty case object, so the toString method for it returns an empty string.

// B. VarListInit takes a VarDeclListStmt object as a parameter and uses its decls property to obtain a list of VarDeclStmt objects.
//    If the list is empty, it returns an empty string. Otherwise, it folds the list from the second element to the end using the first element as the starting accumulator.
//    For each element, it adds the string representation of the element to the accumulator with a comma separator.

// C. VarInit takes a VarDeclStmt object as a parameter and uses its name and expr properties to construct a string representation of the variable declaration.
//    If the expr property is an EmptyExpr, it omits the equal sign and the expression.

// D. ExprInit takes an Expression object as a parameter and returns its string representation.
sealed abstract class ForInit extends AbstractSyntaxTree {
  override def toString = this match {
    case NoneInit() => ""
    case VarListInit(vars) => vars.decls match { 
      case Nil => "" 
      case (a::b) => b.foldLeft(a.toString)((c, e) => { val v = e.asInstanceOf[VarDeclStmt]; c + ", " + v.name + " = " + v.expr }) 
    }
    case VarInit(varDecl) => "var " + varDecl.name + (varDecl.expr match { case EmptyExpr() => "" case _ => " = " + varDecl.expr })
    case ExprInit(expr) => expr.toString
  }
}

// This case class represents an empty initialization.
case class NoneInit() extends ForInit
// This case class represents a variable list initialization and takes a VarDeclListStmt object as its parameter.
case class VarListInit(vars: VarDeclListStmt) extends ForInit
// This case class represents a variable initialization and takes a VarDeclStmt object as its parameter.
case class VarInit(varDecl: VarDeclStmt) extends ForInit
// This case class represents an expression initialization and takes an Expression object as its parameter.
case class ExprInit(expr: Expression) extends ForInit

// auxiliary definition for method calls and assignments
// This is an abstract class that represents an l-value.

// Definition of an abstract class LValue that represents the left-hand side of an assignment.
//  The LValue class has three subclasses, each representing a different kind of l-value:

// A. LVarRef(name): This case class represents an l-value that is a variable reference and takes a string as its parameter.
// The toString method is defined to return the variable name as a string.

// B. LDot(obj, field): This case class represents an l-value that is a dot access and takes an Expression object and a string as its parameters.
// The toString method is defined to return the object followed by a dot and the field name as a string.

// C. LBracket(obj, computeField): This case class represents an l-value that is a bracket access and takes two Expression objects as its parameters.
// The toString method is defined to return the object followed by an opening square bracket, the computed field as an Expression, and a closing square bracket, all as a string.




sealed abstract class LValue extends AbstractSyntaxTree {
  override def toString = this match {
    // This case class represents an l-value that is a variable reference and takes a string as its parameter.
    case LVarRef(name) => name
    // This case class represents an l-value that is a dot access and takes an Expression object and a string as its parameters.
    case LDot(obj, field) => obj + "." + field
    // This case class represents an l-value that is a bracket access and takes two Expression objects as its parameters.
    case LBracket(obj, computeField) => obj + "[" + computeField + "]"
  }
}

case class LVarRef(name : String) extends LValue with VariableAccess
case class LDot(obj : Expression, field : String) extends LValue
case class LBracket(obj : Expression, computeField : Expression) extends LValue

// auxiliary definitions for objects
// This is an abstract class that represents a property of an object.
// Definition of an abstract class called Property that extends AbstractSyntaxTree and overrides the toString method.
// The Property class is meant to be used to represent the different types of properties that can be accessed in an object, such as a variable name, a string, or a number.
//The toString method of the Property class returns a string representation of the object, depending on which subclass it belongs to.
// A. If the object is of type:
//  1. PropVar, the toString method returns the value of the name field.
// B. If the object is of type:
//  2. PropString, the toString method returns the name field enclosed in double quotes.
// C. If the object is of type:
//  3. PropNum, the toString method returns the index field as a string.

abstract class Property extends AbstractSyntaxTree {
  override def toString = this match {
    case PropVar(name) => name
    case PropString(name) => "\"" + name + "\""
    case PropNum(index) => index.toString
  }
}

// This case class represents a property that is a variable reference and takes a string as its parameter.
case class PropVar(name : String) extends Property
// This case class represents a property that is a string and takes a string as its parameter.
case class PropString(name : String) extends Property
// This case class represents a property that is a number and takes a double as its parameter.
case class PropNum(index : Double) extends Property

// This case class represents a key-value pair in an object and takes a Property object and an Expression object as its parameters.
case class ObjectPair(property: Property, expr : Expression) extends AbstractSyntaxTree {
  override def toString = property + " : " + expr
}
