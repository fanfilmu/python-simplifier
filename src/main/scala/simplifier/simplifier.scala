package simplifier

import AST._

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {
  def simplify(node: Node) : Node = {
    node match {
      case n@NodeList(_) => simplify_node_list(n)
      case n@WhileInstr(_,_) => simplify_while_instr(n)
      case _ => node
    }
  }

  def simplify_node_list(listNode: NodeList) : Node = (
    new NodeList(listNode.list.map(x => simplify(x)).filter(_ != null))
  )

  def simplify_while_instr(loop: WhileInstr) : Node = (
    simplify(loop.cond) match {
      case FalseConst() => null
      case _ => WhileInstr(simplify(loop.cond), simplify(loop.body))
    }
  )
}
