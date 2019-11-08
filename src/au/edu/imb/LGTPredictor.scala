package au.edu.imb

import math._
import Similarity._
import util.control.Exception


/**
 * Predictors of LGT events.
 */
abstract class LGTPredictor {
  type Metric = (Seq[Double],Seq[Boolean]) => Double
  type Pair = (SequenceNode,SequenceNode)

  /**
   * Predicts LGT events
   * @param nodes  List of sequence-name2node to predict LGT events inbetween
   * @return Returns list of sequence-node pairs with an event score.
   */
  def predict(nodes:List[SequenceNode]):List[(Pair,Double)]

  /**
   * Computes the performance of the predictor for a given tree and event
   * @param tree Tree with evolved sequences
   * @param event LGT event used with that tree
   * @param metric Performance metric, e.g. Metric.AUC
   * @return Returns the AUC
   */
  def performance(tree:SequenceNode, event:LGTEvent, metric:Metric) = {
    val (pairs,predictions) = predict(tree.leaves).unzip
    val trueEvents = event.leafEvents(tree).toSet
    val targets = pairs.map{case (a,b) => trueEvents.contains(Set(a,b))}
    //for((p,t,pr) <- (pairs,targets,predictions).zipped.toList) printf("%s-%s %s %.3f\n",p._1.name,p._2.name,t,pr)
    metric(predictions, targets)
  }

  /** Returns list with all node pairs (triangular matrix) */
  def pairedNodes(nodes:List[SequenceNode]) =
    (for((ni,i) <- nodes.zipWithIndex; (nj,j) <- nodes.zipWithIndex if i<j) yield (ni,nj)).toList
}

/**
 * A simple predictor that uses a sliding window approach.
 * Distances between the aligned sequences are computed. Then a window slides
 * over the aligned sequences, distances between sequence windows are computed
 * and then the differences between the overall distances and the window based
 * distances are computed. The means over those differences are returned as
 * scores.
 */
class LGTPredictorAlign(method:String, dist:String, w:Int=10) extends LGTPredictor {
  val f = method match {
    case "max" => (v:Seq[Double]) => {VectorMath.max(v)-VectorMath.mean(v)}
    case "min" => (v:Seq[Double]) => {VectorMath.mean(v)-VectorMath.min(v)}
    case "std" => VectorMath.std(_)
  }
  val f_dist = dist match {
    case "hm" => hamming(_,_)
    case "eu" => euclidean(_,_)
    case "ld" => logDet(_,_)
    case "lc" => lcs(_,_)
    case "ff" => fft(_,_)
    case ng => ngram(_:MolecularSequence,_:MolecularSequence,ng.drop(2).toInt)
  }

  def predict(nodes:List[SequenceNode]) = {
    val pairs = pairedNodes(nodes)
    val len = nodes.head.sequence.length
    //val w = max(1,len/100)  // sliding window
    val step = max(1,w/2)  // step size

    def distances(from:Int, until:Int) =
      pairs.map{case (a,b) => f_dist(a.sequence(from,until), b.sequence(from,until))}

    val D = distances(0,len)
    val wD = (0 to (len-step) by step) map (i => distances(i,i+w))
    val wDiffs = wD map (VectorMath.absdiff(D,_))
    val scores = wDiffs.transpose.map(f)

    pairs zip scores
  }

  override def toString = "A-"+method+":"+dist+":"+w
}


class LGTPredictorAlign2(method:String, w:Int=100) extends LGTPredictor {

  def predict(nodes:List[SequenceNode]) = {
    val pairs = pairedNodes(nodes)

    def dist(s1:Seq[Char], s2:Seq[Char]) =
      (s1 zip s2).count{case (a,b) => a!=b} / s1.length.toDouble

    def score(n1:SequenceNode,n2:SequenceNode) = {
      val frags1 = n1.sequence.grouped(w)
      val frags2 = n2.sequence.grouped(w)
      val histogram = (frags1 zip frags2).map{case (a,b) => dist(a,b)}.toSeq
      method match {
        case "max" => VectorMath.max(histogram)-VectorMath.mean(histogram)
        case "std" => VectorMath.std(histogram)
      }
    }

    val scores = pairs.map{case (n1,n2) => score(n1,n2)}
    pairs zip scores
  }

  override def toString = "A2-"+method+":"+w
}


class LGTPredictorNGram(method:String,n:Int, nBins:Int) extends LGTPredictor {
  type NGrams = Map[String,Int]

  def ngrams(s:MolecularSequence):NGrams =
    s.letters.sliding(n).zipWithIndex.toMap

  def positions(ngrams1:NGrams, ngrams2:NGrams):List[Int] = {
    def pos(ngrams1:NGrams, ngrams2:NGrams) =
      (ngrams1.keySet & ngrams2.keySet).toList.map(ngrams2)
    pos(ngrams1,ngrams2):::pos(ngrams2,ngrams1)
  }

  def histogram(positions:Iterable[Int], minPos:Int, maxPos:Int):Seq[Double] = {
    def index(pos:Int) = ((nBins-1.0)*(pos-minPos)/(maxPos-minPos)).toInt
    val bins = Array.fill(nBins)(0.0)
    positions.foreach(p => bins(index(p)) += 1.0)
    bins
  }

  def score(histogram:Seq[Double]) = method match {
    case "max" => VectorMath.max(histogram)-VectorMath.mean(histogram)
    case "std" => VectorMath.std(histogram)
  }

  def predict(nodes:List[SequenceNode]):List[(Pair,Double)] = {
    val pairs = pairedNodes(nodes)
    val node2ngrams = nodes.map(n => (n,ngrams(n.sequence))).toMap
    val pairedNgrams = pairs.map{case (a,b) => (node2ngrams(a),node2ngrams(b))}

    val pos = pairedNgrams.map{case (a,b) => positions(a,b)}
    val posFlat = pos.flatten
    if(posFlat.isEmpty) println("LGTPredictorNGram: no positions => no ngram matches!")
    val (minPos, maxPos) = if(posFlat.isEmpty) (0,0) else (posFlat.min, posFlat.max)
    val scores = pos.map(p => score(histogram(p,minPos,maxPos)))
    pairs zip scores
  }

  override def toString = "N-"+method+":"+n+":"+nBins
}


class LGTPredictorNGram2(method:String,n:Int, nBins:Int) extends LGTPredictor {
  type NGrams = Map[String,Int]

  val f = method match {
    case "max" => (v:Seq[Double]) => {VectorMath.max(v)-VectorMath.mean(v)}
    case "std" => VectorMath.std(_)
  }

  def ngrams(s:MolecularSequence):NGrams =
    s.letters.sliding(n).zipWithIndex.toMap

  def positions(ngrams1:NGrams, ngrams2:NGrams):List[Int] = {
    def pos(ngrams1:NGrams, ngrams2:NGrams) =
      (ngrams1.keySet & ngrams2.keySet).toList.map(ngrams2)
    pos(ngrams1,ngrams2):::pos(ngrams2,ngrams1)
  }

  def histogram(positions:Iterable[Int], minPos:Int, maxPos:Int):Seq[Double] = {
    def index(pos:Int) = ((nBins-1.0)*(pos-minPos)/(maxPos-minPos)).toInt
    val bins = Array.fill(nBins)(0.0)
    positions.foreach(p => bins(index(p)) += 1.0)
    val nPos = positions.size.toDouble/nBins
    for (i <- 0 until nBins)
      bins(i) = abs(bins(i)-nPos)
    bins
  }

  def score(histogram:Seq[Double]) = f(histogram)

  def predict(nodes:List[SequenceNode]):List[(Pair,Double)] = {
    val pairs = pairedNodes(nodes)
    val node2ngrams = nodes.map(n => (n,ngrams(n.sequence))).toMap
    val pairedNgrams = pairs.map{case (a,b) => (node2ngrams(a),node2ngrams(b))}

    def Min(x:List[Int]) = if(x.isEmpty) 0 else x.min
    def Max(x:List[Int]) = if(x.isEmpty) 0 else x.max
    val pos = pairedNgrams.map{case (a,b) => positions(a,b)}
    val scores = pos.map(p => score(histogram(p,Min(p),Max(p))))
    pairs zip scores
  }

  override def toString = "N2-"+method+":"+n+":"+nBins
}


// Longest common substring
class LGTPredictorLCS(method:String, n:Int) extends LGTPredictor {

  // http://en.wikipedia.org/wiki/Longest_common_substring_problem
  def LCS(a:MolecularSequence, b:MolecularSequence) = {
    val L = Array.ofDim[Int](a.length+1,b.length+1)
    for(i <- 1 to a.length; j <- 1 to b.length)
      L(i)(j) = if(a(i-1)!=b(j-1)) 0 else L(i-1)(j-1)+1
    (1 to a.length).map(L(_).max.toDouble).toSeq
  }

  def predict(nodes:List[SequenceNode]):List[(Pair,Double)] = {
    val pairs = pairedNodes(nodes)

    def score(n1:SequenceNode,n2:SequenceNode) = {
      val lcs = LCS(n1.sequence,n2.sequence)
      val bins = lcs.grouped(n1.sequence.length/n).map(_.sum).toSeq
      //println(bins)
      method match {
        case "max" => VectorMath.max(bins)-VectorMath.mean(bins)
        case "std" => VectorMath.std(bins)
      }
    }

    val scores = pairs.map{case (n1,n2) => score(n1,n2)}
    pairs zip scores
  }

  override def toString = "L-"+method+":"+n
}


// fast fourier transform
class LGTPredictorFFT(method:String, n:Int) extends LGTPredictor {
  import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D

  val nuc2vec = Map('A' -> List(1,0,0,0),'C' -> List(0,1,0,0),'T' -> List(0,0,1,0),'G' -> List(0,0,0,1) )

  def seq2vector(s:MolecularSequence) = {
    val c = nuc2vec('A').length
    val n = pow(2,(log(s.length*c)/log(2)).ceil).toInt
    s.map(nuc2vec).flatten.map(_.toDouble).padTo(n,0.0).toArray
  }

  // convolution of fft transformed sequences
  // see: http://code.google.com/p/scalalab/wiki/JTransformsInScalaLab
  // see: http://dsp.stackexchange.com/questions/736/how-do-i-implement-cross-correlation-to-prove-two-audio-files-are-similar
  def correlation(a:MolecularSequence, b:MolecularSequence) = {
    val v1 = seq2vector(a)
    val v2 = seq2vector(b)
    val n = v1.length
    val dfft = new DoubleFFT_1D(n)
    dfft.realForward(v1)
    dfft.realForward(v2)
    val out = Array.ofDim[Double](n*2)
    (0 until n).foreach(i => out(i) = v1(i)*v2(n-i-1))
    dfft.realInverseFull(out, true)
    out.take(n).toSeq
  }


  def predict(nodes:List[SequenceNode]):List[(Pair,Double)] = {
    val pairs = pairedNodes(nodes)

    def score(n1:SequenceNode,n2:SequenceNode) = {
      var corr = correlation(n1.sequence,n2.sequence)
      method match {
        case "max" => VectorMath.max(corr)-VectorMath.mean(corr)
        case "std" => VectorMath.std(corr)
      }
    }

    val scores = pairs.map{case (n1,n2) => score(n1,n2)}
    pairs zip scores
  }

  override def toString = "F-"+method+":"+n
}




/**
 * A random LGT predictor as control.
 */
class LGTPredictorControl extends LGTPredictor {
  def predict(nodes:List[SequenceNode]) =
    pairedNodes(nodes) map {p => (p,random)}
  override def toString = "Ctrl"
}
