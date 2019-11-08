package au.edu.imb

import scala.math.{random,log}
import scala.util.Random.shuffle
import scala.collection.mutable.Stack


/**
 * Creates simulations of phylogenetic trees.
 */
object TreeFactory {

  /**
   * Creates random trees by a method described in
   * A simulation comparison of phylogeny algorithms under equal and unequal evolutionary rates.
   * Kuhner MK, Felsenstein J
   * Mol Biol Evol. 1994 May;11(3):459-68.
   * http://www.ncbi.nlm.nih.gov/pubmed/8015439
   * @param n Number of leaves
   * @return Returns the root node of the tree.
   */
  def KuhnerFelsenstein(n:Int):DefaultNode = {

    /** percent point function (PPF) = inverse of  cumulative distribution function (CDF),
      * which is the CDF for the exponential distribution in this case
      * @param p probability [0,1]
      * @param k shape parameter. Must be > 0.
      */
    def ppf(p:Double, k:Double) = -log(1-p)/k

    /** branch length sampled from exponential distribution PDF = k*exp(-k*x), EXP(PDF) = 1/k */
    def length(k:Double) = ppf(random,k)

    var stack = Stack( (0 until n).map(i => DefaultNode("L"+i, length(n), Nil)):_* )
    for(k <- n until 1 by -1) {
      stack = shuffle(stack)
      stack.push(DefaultNode("I"+(k-2), length(k), List(stack.pop(),stack.pop())))
    }
    stack.pop()
  }

}
