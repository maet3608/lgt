package au.edu.imb

/**
 * Deals with n-gram representations of genomes.
 * Writes and read files of n-grams.
 */

import math.min
import java.io.{BufferedWriter, FileWriter}
import io.Source._


object NGrams {
  type NGramMap = Map[String, Seq[Int]]

  /** Returns a map of n-grams to their sequence positions within the genomic sequence */
  def create(genome:Genome, n:Int, steps:Int=1):NGramMap = {
    var ngramMap = Map[String, List[Int]]().withDefaultValue(List[Int]())
    for (i <- 0 until genome.sequence.length-n by steps) {
      val ngram = genome.sequence.slice(i,i+n)
      ngramMap += ngram -> (i::ngramMap(ngram))
    }
    ngramMap
  }

  /** Saves n-grams and their sequence positions within the specified file */
  def save(filepath:String, ngrams:NGramMap) {
    val writer = new BufferedWriter(new FileWriter(filepath))
    for ((ngram, positions) <- ngrams)
       writer.write(positions.mkString(ngram+"\t",",","\n"))
    writer.close()
  }

  /** Loads a map of n-grams from the specified file */
  def load(filepath:String):NGramMap = {
    var ngramMap = Map[String, Seq[Int]]()
    for (line <- fromFile(filepath).getLines()) {
      val Array(ngram, posstr) = line.split('\t')
      val positions = posstr.split(',').map(_.toInt).toSeq
      ngramMap += ngram -> positions
    }
    ngramMap
  }

  /** Return the n-gram similarity of two sets of n-grams */
  def similarity(ngrams1:Set[String], ngrams2:Set[String]) =
    ngrams1.intersect(ngrams2).size.toDouble / min(ngrams1.size,ngrams2.size)

  /** Returns the n-gram similarity of two n-gram maps */
  def similarity(ngrams1:NGramMap, ngrams2:NGramMap):Double =
    similarity(ngrams1.keySet, ngrams2.keySet)

  // prints out some stats about the collected n-grams
  def info(ngrams:NGramMap) {
    printf("number ngrams  : %d\n", ngrams.size)
    printf("max freq ngrams: %d\n", ngrams.map{case (k,v) => v.size}.max)
    printf("max freq ngram : %s\n", ngrams.maxBy{case (k,v) => v.size}._1)
    printf("avg freq ngrams: %f\n", ngrams.map{case (k,v) => v.size}.sum.toDouble/ngrams.size)
  }


  def main(args:Array[String]) {
    println("running NGrams ...")
    val genome = Genome.load("f:/LGTEvent/genomes/test/AC_000091.fna")
    val ngrams1 = NGrams.create(genome, 20)
    NGrams.info(ngrams1)
    save("f:/LGTEvent/genomes/test/AC_000091.ngrams", ngrams1)
    val ngrams2 = NGrams.load("f:/LGTEvent/genomes/test/AC_000091.ngrams")
    println(ngrams1==ngrams2)
    println("finished.")
  }

}
