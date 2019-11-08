package au.edu.imb

import math.max
import au.edu.imb.Similarity._


/**
 * Test whether generated trees are ultrametric
 * metric:
 *   d(x,y) > 0                for x != y
 *   d(x,y) == 0               for x == y
 *   d(x,y) == d(y,x)          for All x,y
 *   d(x,y) <= d(x,z)+d(y,z)   for All x,y,z (triangle inequality)
 *
 * ultrametric:
 *   d(x,y) <= max(d(x,z),d(y,z)) for All x,y,z  (strong triangle inequality)
 *
 */
object Ultrametric extends App {

  def isUltrametric(x:SequenceNode, y:SequenceNode, z:SequenceNode) = {
    val dxy = hamming(x.sequence, y.sequence)
    val dxz = hamming(x.sequence, z.sequence)
    val dyz = hamming(y.sequence, z.sequence)
    //dxy <= dxz+dyz       // triangle inequality <= metric
    dxy <= max(dxz,dyz)    // strong triangle inequality <= ultrametric
  }


  println("running...")

  val seqLength = 1000
  val treeSize = 8
  for(i <- 0 to 100) {
    val rootSeq = MolecularSequence(seqLength,DNA)
    val baseTree = TreeFactory.KuhnerFelsenstein(treeSize)
    val evolver = new BasicEvolver(Kimura(_))
    val seqTree = evolver.evolve(rootSeq,baseTree)
    val leaves = seqTree.leaves
    val result = (for (x<-leaves; y<-leaves; z<-leaves) yield isUltrametric(x,y,z)).forall(identity)
    println(i,result)
  }

  println("finished.")
}
