package au.edu.imb

import scala.math._
import cern.colt.matrix.linalg.Algebra
import cern.colt.matrix.linalg.SeqBlas
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D

/**
 * Different method to measure sequence similarity
 */
object Similarity {
  // used for logDet only
  private val algebra = new Algebra()
  private val F = new DenseDoubleMatrix2D(4,4)

  // used for fft only
  private val nuc2vec = Map('A' -> List(1,0,0,0),'C' -> List(0,1,0,0),'T' -> List(0,0,1,0),'G' -> List(0,0,0,1) )

  /** Returns hamming distance scaled to sequence length.
    * Requires equal length sequences */
  def hamming(s1:MolecularSequence, s2:MolecularSequence) =
    (s1 zip s2).count{case (a,b) => a!=b} / s1.length.toDouble

  /** Returns euclidean distance */
  def euclidean(s1:MolecularSequence, s2:MolecularSequence) =
    sqrt(hamming(s1,s2))


  /** Returns ngram distance */
  def ngram(s1:MolecularSequence, s2:MolecularSequence, n:Int) = {
    def ngrams(s:MolecularSequence) = s.letters.sliding(n).toSet
    val ngrams1 = ngrams(s1)
    val ngrams2 = ngrams(s2)
    //(ngrams1 & ngrams2).size.toDouble / min(ngrams1.size,ngrams2.size) // local
    (ngrams1 & ngrams2).size.toDouble / max(ngrams1.size,ngrams2.size)  // global
  }

  /** longest common subsequence
   * http://en.wikipedia.org/wiki/Longest_common_substring_problem
   * */
  def lcs(s1:MolecularSequence, s2:MolecularSequence) = {
    val L = Array.ofDim[Int](s1.length+1,s2.length+1)
    for(i <- 1 to s1.length; j <- 1 to s2.length)
      L(i)(j) = if(s1(i-1)!=s2(j-1)) 0 else L(i-1)(j-1)+1
    (1 to s1.length).map(L(_).max.toDouble).sum
  }

  // convolution between the fast fourier transformed sequences
  // see: http://code.google.com/p/scalalab/wiki/JTransformsInScalaLab
  def fft(s1:MolecularSequence, s2:MolecularSequence) = {
    def seq2vector(s:MolecularSequence) = {
      val c = nuc2vec('A').length
      val n = pow(2,(log(s.length*c)/log(2)).ceil).toInt
      s.map(nuc2vec).flatten.map(_.toDouble).padTo(n,0.0).toArray
    }
    val v1 = seq2vector(s1)
    val v2 = seq2vector(s2)
    val n = v1.length
    val dfft = new DoubleFFT_1D(n)
    dfft.realForward(v1)
    dfft.realForward(v2)
    val out = Array.ofDim[Double](n*2)
    (0 until n).foreach(i => out(i) = v1(i)*v2(i))
    dfft.realInverseFull(out, true)
    out.max
  }



  /** Returns logDet distance:
    * http://www.ch.embnet.org/CoursEMBnet/PHYL03/Slides/Distance_membley.pdf
    * Lockhardt et al.(1994) Mol. Biol.Evol.11:605-612
    * Lake (1994) PNAS 91:1455-1459 (paralinear distances)
    */
  def logDet(s1:MolecularSequence, s2:MolecularSequence) = {
    def idx(letter:Char) = s1.alphabet.toIndex(letter)
    F.assign(0)
    for ((a,b) <- s1 zip s2)
      F.setQuick(idx(a),idx(b),F.getQuick(idx(a),idx(b))+1)
    SeqBlas.seqBlas.dscal(1.0/s1.length,F)
    //-log(algebra.det(F))   // terrible performance with log
    algebra.det(F)
  }
}


/** Usage example */
object SimilarityExample extends App {
  val s1 = MolecularSequence("AAAAAAAAAA", DNA)
  val s2 = MolecularSequence("AAAAAAAACC", DNA)
  println(Similarity.hamming(s1,s2))
}