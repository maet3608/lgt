package au.edu.imb

import org.scalatest.FunSuite

/**
 * Unit test for LGT events
 */
class LGTEventTest extends FunSuite  {

  class Node(val name:String, val length:Double, val descendants:List[Node]) extends AbstractNode[Node] {
    override def toString = name
  }

  object Node {
    def apply(name:String, descendants:List[Node]) = new Node(name,0.0,descendants)
    def apply(name:String) = new Node(name,0.0,Nil)
  }

  class LGTNoAction extends LGTAction {
    def perform(srcSeq:MolecularSequence, destSeq:MolecularSequence) = destSeq
  }

  test("leaf events") {
    val A = Node("A")
    val B = Node("B")
    val C = Node("C")
    val D = Node("D")
    def tree = Node("R", List(Node("X",List(A,B)), Node("Y",List(C,D))))

    def createEvent(srcName:String, destName:String) =
      new LGTEvent(srcName,0.0, destName,0.0, new LGTNoAction)

    expect(List(Set(A,B)))(createEvent("A","B").leafEvents(tree))
    expect(List(Set(A,B)))(createEvent("B","A").leafEvents(tree))
    expect(List(Set(A,D), Set(B,D)))(createEvent("X","D").leafEvents(tree))
  }

  test("perform") {
    val srcSeq = MolecularSequence("A"*20, DNA)
    val destSeq = MolecularSequence("C"*20, DNA)
    val event = new LGTEvent("Dummy",0.0, "Dummy",0.0, new LGTActionOverwrite(5))
    val eventSeq = event.perform(srcSeq, destSeq)
    assert( eventSeq.letters.contains("A"*5) )
  }

}