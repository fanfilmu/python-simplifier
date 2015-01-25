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
      case n@Unary(_,_) => simplify_unary_exp(n)
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

  def simplify_unary_exp(unary: Unary) : Node = (
    simplify(unary.expr) match {
      case FalseConst() if unary.op == "not" => TrueConst()
      case TrueConst() if unary.op == "not" => FalseConst()
      case u@Unary(_,_) if u.op == unary.op => u.expr
      case _ => Unary(unary.op,simplify(unary.expr))
    }
  )
}
