package hwk

import scala.collection.mutable.Set
import scala.collection.mutable.Queue

// stmt: a program with CFG already built
case class Analysis(stmt: Statement) {
  val stmts = cfgNodes.toList // the list of all CFG statements
  val map = stmts.map(s => (s, Node(s))).toMap // map from CFG statements to nodes
  val nodes = map.values.toList // all CFG nodes

  // collect the CFG statements from a program
  private def cfgNodes: Set[Statement] = {
    def h(stmt: Statement, sofar: Set[Statement]) {
      sofar += stmt

      for (s <- stmt.succ; if !sofar.contains(s)) h(s, sofar)
    }
    val sofar = Set[Statement]()
    h(stmt.entry, sofar)
    sofar
  }

  // helper methods to retrieve the successors and predecessors of a CFG node
  def succ(node: Node) = node.stmt.succ.map(s => map(s))
  def pred(node: Node) = node.stmt.pred.map(s => map(s))

  // all variables assigned in this program
  val variables: Set[String] = vars(this.stmts)

  private def vars(stmt: Statement): Set[String] = {
    stmt match {
      case Script(stmts)                          => vars(stmts)
      case BlockStmt(stmts)                       => vars(stmts)
      case VarDeclListStmt(stmts)                 => vars(stmts)
      case VarDeclStmt(x, e)                      => Set(x.str)
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => Set(n)
      case IfStmt(_, t, e)                        => vars(t).union(vars(e))
      case WhileStmt(_, b)                        => vars(b)
      case DoWhileStmt(_, b)                      => vars(b)
      case SwitchStmt(_, cases, d)                => (d match { case Some(c) => c :: cases case None => cases }).map(c => vars(c)).reduce((a, b) => a.union(b))
      case CaseStmt(_, s)                         => vars(s)
      case _                                      => Set()
    }
  }

  private def vars(stmts: List[Statement]): Set[String] = stmts.map(s => vars(s)).reduce((a, b) => a.union(b))

  // worklist algorithm to compute reaching definitions at the entry/exit of each CFG node
  def worklist {
    // TODO: you can just implement this

    // Initialize the entry and exit sets for each node to be empty.
    nodes.foreach(node => {
      node.entry.clear()
      node.exit.clear()
    })

    // Create an empty queue and add all nodes to the queue.
    val worklist = Queue[Node]()
    print(worklist)
    nodes.foreach(node => {
      if (!pred(node).isEmpty) {
        worklist.enqueue(node)
      }
    })

    //nodes.foreach(node => if (!pred(node).isEmpty) worklist.enqueue(node))

    // worklist.head.entry ++ variables.map((_, -1L))
    // print(worklist.head)
    // Perform the work list algorithm.
    while (!worklist.isEmpty) {
      val node = worklist.dequeue()
      val oldEntry = node.entry.clone()
      val oldExit = node.exit.clone()

      // Compute the new entry set as the union of the exit sets of all predecessors.
      node.entry.clear()
      pred(node).foreach(predNode => node.entry ++= predNode.exit)

      // Handle the special case of the entry point for the empty predecessor.
      if (pred(node).isEmpty) {
        setEntryForEmptyPred(node)
      }

      // Compute the new exit set as the union of the node's entry set and the definitions assigned in this node.
      node.exit.clear()
      node.exit ++= node.entry
      val (killSet, genSet) = computeKillGenSets(node)
      node.exit ++= genSet
      node.exit --= killSet

      // If the entry or exit set has changed, add all successors to the work list.
      if (node.entry != oldEntry || node.exit != oldExit) {
        succ(node).foreach(succNode => worklist.enqueue(succNode))
      }
    }
  }
  
  // Cases
  def computeKillGenSets(node: Node): (Set[(String, Long)], Set[(String, Long)]) = node.stmt match {
    case VarDeclStmt(x, _)                      => (Set((x.str, -1L)), Set((x.str, node.stmt.id)))
    case ExprStmt(AssignExpr(_, LVarRef(n), _)) => (Set((n, -1L)), Set((n, node.stmt.id)))
    case _                                      => (Set(), Set())
  }
  
  // Special case to check if current node has predecessor as none
  def setEntryForEmptyPred(node: Node): Unit = {
    node.entry ++= Set.empty[(String, Long)]
    succ(node).foreach(succNode => {
      if (pred(succNode).size == 1 && pred(succNode).head == node) {
        // If the successor node only has one predecessor and that predecessor is the current node,
        // update its entry set with the empty set {}.
        succNode.entry.clear()
        succNode.entry ++= Set.empty[(String, Long)]
        // succNode.entry ++= variables.map((_, -1L))
      }
    })
  }

  // Prior Implementations Ignore

  /*def worklist {
  // Initialize the entry and exit sets for each node to be empty.
  nodes.foreach(node => {
    node.entry.clear()
    node.exit.clear()
  })

  // Create an empty queue and add all nodes to the queue.
  val worklist = Queue[Node]()
  nodes.foreach(node => {
    if (pred(node).isEmpty) {
      setEntryForEmptyPred(node) // Call the helper function to handle the special case of the entry point for the empty predecessor.
      worklist.enqueue(node)
    }
  })

  // Perform the worklist algorithm.
  while (!worklist.isEmpty) {
    val node = worklist.dequeue()
    val oldEntry = node.entry.clone()
    val oldExit = node.exit.clone()

    // Compute the new entry set as the union of the exit sets of all predecessors.
    node.entry.clear()
    pred(node).foreach(predNode => node.entry ++= predNode.exit)

    // Compute the new exit set as the union of the node's entry set and the definitions assigned in this node.
    node.exit.clear()
    node.exit ++= node.entry
    node.stmt match {
      case VarDeclStmt(x, _)                      => node.exit += ((x.str, node.stmt.id))
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => node.exit += ((n, node.stmt.id))
      case _                                      =>
    }

    // If the entry or exit set has changed, add all successors to the worklist.
    if (node.entry != oldEntry || node.exit != oldExit) {
      succ(node).foreach(succNode => worklist.enqueue(succNode))
    }
  }
}


// Helper function to handle the special case of the entry point for the empty predecessor.
def setEntryForEmptyPred(node: Node) {
  node.entry ++= Set()
}*/

  // Attempted the Transition Functions and MOP and MFP

  /*
  // compute the MFP solution for the out set
def MFPout(l: Node): Set[(String, Int)] = {
  // Initialize the entry and exit sets for each node to be empty.
  nodes.foreach(node => {
    node.entry.clear()
    node.exit.clear()
  })

  // Create an empty queue and add all nodes to the queue.
  val worklist = Queue[Node]()
  nodes.foreach(node => worklist.enqueue(node))

  // Perform the worklist algorithm.
  while (!worklist.isEmpty) {
    val node = worklist.dequeue()
    val oldEntry = node.entry.clone()
    val oldExit = node.exit.clone()

    // Compute the new entry set as the union of the exit sets of all predecessors.
    node.entry.clear()
    pred(node).foreach(predNode => node.entry ++= predNode.exit)

    // Compute the new exit set as the union of the node's entry set and the definitions assigned in this node.
    node.exit.clear()
    node.exit ++= node.entry
    node.stmt match {
      case VarDeclStmt(x, _)                      => node.exit += ((x.str, node.stmt.id))
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => node.exit += ((n, node.stmt.id))
      case _                                      =>
    }

    // If the exit set has changed, add all successors to the worklist.
    if (node.exit != oldExit) {
      succ(node).foreach(succNode => worklist.enqueue(succNode))
    }
  }

  // return the MFP solution for the out set at node l
  val paths = pathPlus(l)
  val solutions = paths.map(p => MFPPath(l, p))
  solutions.foldLeft(Set[(String, Int)]())((acc, s) => acc.union(s))
}*/

  /*// compute the MFP solution for the in set
def MFPin(l: Node): Set[(String, Int)] = {
  // Initialize the entry and exit sets for each node to be empty.
  nodes.foreach(node => {
    node.entry.clear()
    node.exit.clear()
  })

  // Create an empty queue and add all nodes to the queue.
  val worklist = Queue[Node]()
  nodes.foreach(node => worklist.enqueue(node))

  // Perform the worklist algorithm.
  while (!worklist.isEmpty) {
    val node = worklist.dequeue()
    val oldEntry = node.entry.clone()
    val oldExit = node.exit.clone()

    // Compute the new exit set as the union of the entry sets of all successors.
    node.exit.clear()
    succ(node).foreach(succNode => node.exit ++= succNode.entry)

    // Compute the new entry set as the union of the node's exit set and the definitions assigned in this node.
    node.entry.clear()
    node.entry ++= node.exit
    node.stmt match {
      case VarDeclStmt(x, _)                      => node.entry += ((x.str, node.stmt.id))
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => node.entry += ((n, node.stmt.id))
      case _                                      =>
    }

    // If the entry set has changed, add all predecessors to the worklist.
    if (node.entry != oldEntry) {
      pred(node).foreach(predNode => worklist.enqueue(predNode))
    }
  }

  // return the MFP solution for the in set at node l
  val paths = pathStar(l)
  val solutions = paths.map(p*/

  // make a dot graph with entry/exit reaching definition of every node
  def toDotGraph = {
    val entry = (s: Statement) => map(s).entry.toList.sortBy(x => x).mkString(" ")
    val exit = (s: Statement) => map(s).exit.toList.sortBy(x => x) mkString (" ")
    val labels = stmts.map(s => s"${s.id} [label = ${s.dotStr}, xlabel = ${"\""}${entry(s)}\\n${exit(s)}${"\""}]")
    s"digraph {\n${(labels ++ stmt.toDot).reduceLeft((c, e) => c + "\n" + e)}\n}"
  }
}

// CFG node to hold the reaching definitions at the entry/exit of a CFG statement
case class Node(stmt: Statement) {
  val entry = Set[(String, Long)]()
  val exit = Set[(String, Long)]()
}
 
