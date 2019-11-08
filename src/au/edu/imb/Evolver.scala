package au.edu.imb

/**
 * Evolves sequences along a tree
 */


/**
 * Base class for evolvers using substitution models
 * @param smf Substitution model factory. A function that returns a substitution model
 *            for a given branch length.
 */
abstract class Evolver(smf:(Double) => SubstitutionModel) {
  /**
   * Evolves a sequence along a phylogenetic tree.
   * @param sequence Root sequence to evolve.
   * @param node Root node of  phylogenetic tree
   * @tparam T Type of name2node.
   * @return Returns root of the sequence tree
   */
  def evolve[T](sequence:MolecularSequence, node:AbstractNode[T]):SequenceNode
}


/**
 * An evolver with out LGT events.
 * @param smf Substitution model factory. A function that returns a substitution model
 *            for a given branch length.
 */
class BasicEvolver(smf:(Double) => SubstitutionModel) extends Evolver(smf) {

  def evolve[T](sequence:MolecularSequence, node:AbstractNode[T]):SequenceNode = {
    def descendants = node.descendants.map{n => evolve(smf(n.length)(sequence),n)}
    new SequenceNode(node.name, node.length, sequence, descendants)
  }
}


/**
 * An evolver with LGT events.
 * @param smf Substitution model factory. A function that returns a substitution model
 *            for a given branch length.
 * @param event LGT event.
 */
class LGTEvolver(smf:(Double) => SubstitutionModel, event:LGTEvent) extends Evolver(smf) {

  /** Returns path from node to node with the given name or empty list otherwise */
  def path[T](node:AbstractNode[T], name:String):List[AbstractNode[T]] = {
    if(node.name == name) return List(node)
    for(descendant <- node.descendants) {
      val p = path(descendant,name)
      if(!p.isEmpty) return node::p
    }
    List()
  }

  /** Evolves the given sequence along the phylogentic tree given by its root node.
    * Returns root node of sequence tree */
  def evolve[T](sequence:MolecularSequence, node:AbstractNode[T]):SequenceNode = {
    var lgtSeq:MolecularSequence = null

    def evolve_(sequence:MolecularSequence, node:AbstractNode[T], path:List[AbstractNode[T]]):SequenceNode = {
      def sub(sequence:MolecularSequence, node:AbstractNode[T], c:Double=1.0) = smf(node.length*c)(sequence)

      import event.{srcC,destC}
      val seq = node.name match {
        case event.srcName => lgtSeq = sub(sequence,node,srcC); sub(lgtSeq,node,1-srcC)
        case event.destName => sub(event.perform(lgtSeq,sub(sequence,node,destC)),node,1-destC)
        case _ => sequence
      }

      val (descendants, subPath) = path match {
        case h::t if node.descendants.contains(h) => (h::node.descendants.filter(_!=h), t)
        case _ => (node.descendants, path)
      }

      def newDescendants = descendants.map{n => evolve_(sub(seq,n),n,subPath)}
      new SequenceNode(node.name, node.length,seq, newDescendants)
    }

    evolve_(sequence, node, path(node,event.srcName).tail)
  }
}


/** Usage example */
object EvolverExample extends App {
  val event = new LGTEvent("X",0.5, "Y",0.5, new LGTActionOverwrite(10))
  def smf = JukesCantor(_)
  //val evolver = new BasicEvolver(smf)
  val evolver = new LGTEvolver(smf,event)
  val seq = MolecularSequence(100,DNA)
  //val seq = MolecularSequence("C"*100, DNA)
  println(seq)
  val parser = new SimpleParser(DefaultNode.apply)
  val root = parser.read("R:0.0 (X:0.1(A:0.1 B:0.1) Y:0.1 (C:0.1 D:0.1))")
  root.display(indent=4)
  val tree = evolver.evolve(seq,root)
  tree.display(indent=0)
}
