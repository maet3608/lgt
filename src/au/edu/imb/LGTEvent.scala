package au.edu.imb

import scala.util.Random.shuffle

/**
 * Describes lateral genetic transfer (LGT) events.
 */


/**
 * The actual event to be performed, e.g. inserting a sub-sequence take from a
 * source sequence into the destination sequence.
 */
trait LGTAction {
  protected val random = new scala.util.Random()

  /**
   * Performs the action
   * @param srcSeq  Source sequence
   * @param destSeq Destination sequence
   * @return Returns destination sequence after LGT event.
   */
  def perform(srcSeq:MolecularSequence, destSeq:MolecularSequence):MolecularSequence
}


/** Takes a section of n letters at a random position in srcSeq and
  * overwrites a section at random position in desSeq.
  */
class LGTActionOverwrite(n:Int) extends LGTAction {
  def perform(srcSeq:MolecularSequence, destSeq:MolecularSequence):MolecularSequence = {
    val from = random.nextInt(srcSeq.length-n+1)
    destSeq(0,from)+srcSeq(from,from+n)+destSeq(from+n,destSeq.length)
  }
}


/**
 * Data of the LGT event
 * @param srcName Name of source node within the phylogenetic tree
 * @param srcC Proportion on the branch where the event is happening, e.g. 0.1 = 10% toward the node
 * @param destName Name of dest node within the phylogenetic tree
 * @param destC Proportion on the branch where the event is happening, e.g. 0.1 = 10% toward the node
 * @param action Action of the event, e.g. insertion of genetic material
 */
class LGTEvent (
  val srcName:String,
  val srcC:Double,
  val destName:String,
  val destC:Double,
  action:LGTAction) {

  /** Performs the event.
    * Note that an event cannot go backward in time, e.g. defining an event that copies material
    * from a child to its parent is not possible and an exception is thrown.
    */
  def perform(srcSeq:MolecularSequence, destSeq:MolecularSequence):MolecularSequence = {
    if(srcSeq == null)   throw new Exception("Invalid LGT event: backward in time!")
    if(destSeq == null)  throw new Exception("Invalid LGT event! Destination missing.")
    action.perform(srcSeq,destSeq)
  }

  /**
   * Computes from the source and destination of the original LGT event the
   * ancestral leave name2node that inherit that event and returns all pairings
   * between source leave name2node and destination leave name2node.
   * @param node Root node of tree
   * @tparam T Node type
   * @return Returns list of node pairs as sets
   */
  def leafEvents[T <: AbstractNode[T]](node:AbstractNode[T]) = {
    val nodes = node.name2node
    for(sl <- nodes(srcName).leaves; dl <- nodes(destName).leaves) yield Set(sl,dl)
  }

  override def toString = "LGTEvent: %s -> %s\n" format (srcName,destName)
}

/** Factory for LGT events */
object LGTEvent {
  /** Creates an event with the given parameters */
  def apply(srcName:String,srcC:Double,destName:String,destC:Double,action:LGTAction):LGTEvent =
    new LGTEvent(srcName,srcC,destName,destC,action)

  /** Creates a random LGT event for the given tree */
  def apply(root:DefaultNode,action:LGTAction):LGTEvent = {
    val nodes = root.allDescendants.tail // without root
    val srcNode =  shuffle(nodes).head
    val srcClade = srcNode.allDescendants
    val destClade = nodes.toSet.diff(root.path(srcNode).toSet).diff(srcClade.toSet)
    val destNode = shuffle(destClade.toSeq).head
    LGTEvent(srcNode.name,0.5,destNode.name,0.5,action)
  }
}


/** Usage example */
object LGTEventExample extends App {
  val srcSeq = MolecularSequence("A"*20, DNA)
  val destSeq = MolecularSequence("C"*20, DNA)
  val event = new LGTEvent("Dummy",0.0, "Dummy",0.0, new LGTActionOverwrite(5))
  println(srcSeq)
  println(destSeq)
  println(event.perform(srcSeq, destSeq))
}





