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
      case n@BinExpr(_,_,_) => simplify_bin_expr(n)
      case n@IfInstr(_,_) => simplify_if_instr(n)
      case n@IfElseInstr(_,_,_) => simplify_if_else_instr(n)
      case n@IfElifInstr(_,_,_) => simplify_if_elif_instr(n)
      case n@IfElifElseInstr(_,_,_,_) => simplify_if_elif_else_instr(n)
      case _ => node
    }
  }

  def simplify_node_list(listNode: NodeList) : Node = (
    listNode.list match {
      case List() => listNode
      case List(NodeList(_)) => simplify(listNode.list.head)
      case _ => new NodeList(listNode.list.map(x => simplify(x)).filter(_ != null))
    }
  )

  def simplify_while_instr(loop: WhileInstr) : Node = (
    simplify(loop.cond) match {
      case FalseConst() => null
      case _ => WhileInstr(simplify(loop.cond), simplify(loop.body))
    }
  )

  def simplify_unary_exp(unary: Unary) : Node = (
    (simplify(unary.expr),unary.op) match {
      case (FalseConst(),"not") => TrueConst()
      case (TrueConst(),"not") => FalseConst()
      case (u@Unary(_,_),u.op) => u.expr
      case (b@BinExpr(_,_,_),"not") if b.op == "==" => BinExpr("!=",b.left,b.right)
      case (b@BinExpr(_,_,_),"not") if b.op == "!=" => BinExpr("==",b.left,b.right)
      case (b@BinExpr(_,_,_),"not") if b.op == "<=" => BinExpr(">",b.left,b.right)
      case (b@BinExpr(_,_,_),"not") if b.op == ">=" => BinExpr("<",b.left,b.right)
      case (b@BinExpr(_,_,_),"not") if b.op == "<" => BinExpr(">=",b.left,b.right) 
      case (b@BinExpr(_,_,_),"not") if b.op == ">" => BinExpr("<=",b.left,b.right) 
      case _ => Unary(unary.op,simplify(unary.expr))
    }
  )

  def simplify_bin_expr(binary: BinExpr) : Node = (
    (simplify(binary.left),simplify(binary.right),binary.op) match {
      case (n@IntNum(_),m@IntNum(_),"/") if m.value != 0 => FloatNum(n.value/m.value)
      case (n@IntNum(_),m@IntNum(_),"+") => IntNum(n.value+m.value)
      case (n@IntNum(_),m@IntNum(_),"-") if n.value < m.value => Unary("-",IntNum(-(n.value-m.value)))
      case (n@IntNum(_),m@IntNum(_),"-") => IntNum(n.value-m.value)
      case (n@IntNum(_),m@IntNum(_),"*") => IntNum(n.value*m.value)
      case (n@FloatNum(_),m@FloatNum(_),"/") if m.value != 0 => FloatNum(n.value/m.value)
      case (n@FloatNum(_),m@FloatNum(_),"+") => FloatNum(n.value+m.value)
      case (n@FloatNum(_),m@FloatNum(_),"-") if n.value < m.value => Unary("-",FloatNum(-(n.value-m.value)))
      case (n@FloatNum(_),m@FloatNum(_),"-") => FloatNum(n.value-m.value)
      case (n@FloatNum(_),m@FloatNum(_),"*") => FloatNum(n.value*m.value)
      case (v@Variable(_),x@Variable(_),"=="|">="|"<=") if v.name == x.name => TrueConst()
      case (v@Variable(_),x@Variable(_),"!="|"<"|">") if v.name == x.name => FalseConst()
      case (v@Variable(_),x@Variable(_),"-") if v.name == x.name => IntNum(0)
      case (v@Variable(_),n@IntNum(_),"+" | "-") if n.value == 0 => v
      case (n@IntNum(_),v@Variable(_),"+" | "-") if n.value == 0 => v
      case (v@Variable(_),n@IntNum(_),"*") if n.value == 1 => v
      case (n@IntNum(_),v@Variable(_),"*") if n.value == 1 => v
      case (v@Variable(_),n@IntNum(_),"*") if n.value == 0 => IntNum(0)
      case (n@IntNum(_),v@Variable(_),"*") if n.value == 0 => IntNum(0)
      case (v@_,x@_,"/") if v == x => IntNum(1)
      case (v@Variable(_),x@Variable(_),"or") if v.name == x.name => x
      case (v@Variable(_),x@Variable(_),"and") if v.name == x.name => x
      case (v@Variable(_),FalseConst(),"or") => v
      case (v@Variable(_),FalseConst(),"and") => FalseConst()
      case (v@Variable(_),TrueConst(),"or") => TrueConst()
      case (v@Variable(_),TrueConst(),"and") => v
      case (u@Unary("-",_),v@_,"+") => simplify(BinExpr("-",v,u.expr))
      case (v@_,u@_,o@_) => BinExpr(o, v, u)
    }
  )

  def simplify_if_instr(node: IfInstr) : Node = (
    simplify(node.cond) match {
      case TrueConst() => simplify(node.left)
      case FalseConst() => null
      case c@_ => IfInstr(c, simplify(node.left))
    }
  )

  def simplify_if_else_instr(node: IfElseInstr) : Node = (
    simplify(node.cond) match {
      case TrueConst() => simplify(node.left)
      case FalseConst() => simplify(node.right)
      case c@_ => IfElseInstr(c, simplify(node.left), simplify(node.right))
    }
  )

  def simplify_if_elif_instr(node: IfElifInstr) : Node = (
    simplify(node.cond) match {
      case TrueConst() => simplify(node.left)
      case FalseConst() if node.elifs.size == 1 => simplify(IfInstr(node.elifs.head.cond, node.elifs.head.left))
      case FalseConst() if node.elifs.size > 1 => simplify(IfElifInstr(node.elifs.head.cond, node.elifs.head.left, node.elifs.slice(1, node.elifs.size)))
      case c@_ => IfElifInstr(c, simplify(node.left), node.elifs)
    }
  )

  def simplify_if_elif_else_instr(node: IfElifElseInstr) : Node = (
    simplify(node.cond) match {
      case TrueConst() => simplify(node.left)
      case FalseConst() if node.elifs.size == 1 => simplify(IfElseInstr(node.elifs.head.cond, node.elifs.head.left, node.right))
      case FalseConst() if node.elifs.size > 1 => simplify(IfElifElseInstr(node.elifs.head.cond, node.elifs.head.left, node.elifs.slice(1, node.elifs.size), node.right))
      case c@_ => IfElifElseInstr(c, simplify(node.left), node.elifs, simplify(node.right))
    }
  )
}
