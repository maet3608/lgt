package au.edu.imb

import org.scalatest.FunSuite

/**
 * Unit test for tree name2node.
 */
class NodeTest extends FunSuite  {

  class Node(val name:String, val length:Double, val descendants:List[Node]) extends AbstractNode[Node] {
    def treeString:String =
      if(descendants.isEmpty) name else name + descendants.map(_.treeString).mkString("(",",",")")
    override def toString = name
  }

  object Node {
    def apply(name:String, descendants:List[Node]) = new Node(name,0.0,descendants)
    def apply(name:String) = new Node(name,0.0,Nil)
  }

  def tree1 = Node("R", List(Node("X",List(Node("A"),Node("B"))), Node("Y",List(Node("C"),Node("D")))))
  def tree2 = Node("R", List(Node("X",List(Node("A"),Node("B"))), Node("Y",List(Node("C")))))
  def tree3 = Node("R", List(Node("X",List(Node("B"))), Node("C")))

  val A = Node("A")
  val B = Node("B")
  val C = Node("C")
  val D = Node("D")
  val X = Node("X",List(A,B))
  val Y = Node("Y",List(C,D))
  val R = Node("R", List(X, Y))

  test("constructor") {
    expect("R(X(A,B),Y(C,D))")(tree1.treeString)
    expect("R(X(A,B),Y(C))")(tree2.treeString)
    expect("R(X(B),C)")(tree3.treeString)
  }

  test("leaves") {
    expect("A,B,C,D")(tree1.leaves.mkString(","))
    expect("A,B,C")(tree2.leaves.mkString(","))
    expect("B,C")(tree3.leaves.mkString(","))
  }

  test("path") {
    expect("R,X,A")(R.path(A).mkString(","))
    expect("R,X,B")(R.path(B).mkString(","))
    expect("R,Y,C")(R.path(C).mkString(","))
    expect("R,Y,D")(R.path(D).mkString(","))
    expect("R,X")(R.path(X).mkString(","))
    expect("R,Y")(R.path(Y).mkString(","))
    expect("R")(R.path(R).mkString(","))
  }

  test("allDescendants") {
    expect("R,X,A,B,Y,C,D")(R.allDescendants.mkString(","))
    expect("X,A,B")(X.allDescendants.mkString(","))
    expect("Y,C,D")(Y.allDescendants.mkString(","))
    expect("A")(A.allDescendants.mkString(","))
    expect("B")(B.allDescendants.mkString(","))
    expect("C")(C.allDescendants.mkString(","))
    expect("D")(D.allDescendants.mkString(","))
  }

  test("name2node") {
    expect(true)(tree3.name2node.contains("R"))
    expect(true)(tree3.name2node.contains("X"))
    expect(true)(tree3.name2node.contains("B"))
    expect(true)(tree3.name2node.contains("C"))
  }
}