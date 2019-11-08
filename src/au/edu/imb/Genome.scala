package au.edu.imb

import io.Source._
import math.{sqrt,min}
import java.io.{FileWriter, File}


/**
 * Handles genome sequences.
 */

class Genome(
  val id:String,
  val description:String,
  val filepath:String,
  val sequence:String) {

  /**
   * Aligns two genomes using a sliding window in this genome and counting the n-gram matches
   * and the measuring the spread of matching n-grams within the other genome.
   * @param genome genome to align with
   * @param n n-gram size
   * @param ws sliding window size
   * @return vector of alignment scores with the same length of the sequence
   */
  def align(genome:Genome, n:Int, ws:Int=100): Seq[Double] = {
    val ngram2pos = NGrams.create(genome, n)

    def align(window:String, steps:Int = 1) =
      for (ngram <- window.sliding(n,steps) if ngram2pos.contains(ngram)) yield ngram2pos(ngram)
    def dist(pos:Int, mean:Double) = (pos-mean)*(pos-mean)
    def std(pos:Seq[Int], mean:Double) = sqrt(pos.foldLeft(0.0)(_ + dist(_,mean))/pos.size.toDouble)

    var qs = Seq[Double]()
    for (window <- sequence.sliding(ws)) {
      val positions = align(window).toSeq
      val matches = positions.size/window.size.toDouble  // number of matching n-grams
      val allpos = positions.flatten
      val allmean = if(allpos.size > 0) allpos.sum/allpos.size else 0
      val bestpos = positions.map(ps => ps.minBy(p => dist(p,allmean)))
      val bestmean = if(bestpos.size > 0) bestpos.sum/bestpos.size else 0
      val beststd = std(bestpos,bestmean)
      val Q = if(beststd > 0) matches/beststd else 0.0
      qs = qs:+Q
    }
    qs
  }

  override def toString = "%s %s".format(id, sequence.take(20))
}


object Genome {
  // loads a genome sequence
  def load(filepath:String): Genome = {
    println("loading: "+filepath)
    val lines = fromFile(filepath).getLines()
    val header = lines.next().split('|')
    val sequence = lines.mkString.trim
    new Genome(header(3), header(4), filepath, sequence)
  }

  // loads all genomes from the given folder.
  def loadAll(basepath:String, pattern:String): Seq[Genome] = {
    assert(basepath.endsWith("/"), "Basepath must end with '/'")
    for(fname <- (new File(basepath)).list if fname.matches(pattern)) yield load(basepath+fname)
  }

  def experiment1() {
    val filepath1 =  "f:/LGTEvent/genomes/test/NC_000913_test.fna"
    val genome1 = load(filepath1)
    val anno1 = Annotations.load(filepath1.replace(".fna",".ptt"))
    val profile1 = anno1.geneProfile(genome1.sequence)
    println(genome1.id)
    val filepath2 =  "f:/LGTEvent/genomes/test/NC_002655_test.fna"
    //val filepath2 =  "f:/LGTEvent/genomes/test/NC_000913_test.fna"
    val genome2 = load(filepath2)
    val anno2 =  Annotations.load(filepath2.replace(".fna",".ptt"))
    val profile2 = anno2.geneProfile(genome2.sequence)
    println(genome2.id)
    val qs = genome1.align(genome2, 12, 120)
    val f = new FileWriter("f:/LGTEvent/genomes/test/alignment.txt")
    val minlen = min(profile1.size,profile2.size)
    def same(a:Char, b:Char):Int = if (a==b) 1 else 0
    for ((q,i) <- qs zip (0 until minlen) )
      f.write("%d %d %d %g\n" format (profile1(i), profile2(i), same(genome1.sequence(i), genome2.sequence(i)), q))
    f.close()
  }

  def experiment2() {
    def createNGramSet(genome:Genome, n:Int=12):Set[String] =
      genome.sequence.sliding(n).toSet
    def createProfile(ngram:String, ngramSets:Iterable[Set[String]]):String =
      ngramSets.map(ngrams => if(ngrams.contains(ngram)) "1" else "0").mkString(" ")

    println("loading genomes...")
    val basepath = "f:/LGTEvent/genomes/test/"
    val ngramSets = Genome.loadAll(basepath,".*fna").map(createNGramSet(_))
    val ngramsAll = ngramSets.reduceLeft(_|_)
    println("set sizes: "+ngramSets.map("%d " format _.size).mkString)

    println("compute genetic transfer units...\n")
    val gtus = ngramsAll.groupBy(createProfile(_,ngramSets))
    for ((profile,ngrams) <- gtus.toSeq.sortBy(_._1))
      printf("%s   %d\n",profile,ngrams.size)
    printf("number of gtus:              %d\n",gtus.size)
    printf("number of gtus of size one:  %d\n",gtus.count(_._2.size == 1))
    printf("number of gtus of size <100: %d\n",gtus.count(_._2.size < 100))
    //println("writing profiles...")
    //var profiles = ngramsAll.map(ngram => (ngram,createProfile(ngram,ngramSets))).toSeq.sortBy(_._2)
    //val f = new FileWriter("f:/LGTEvent/genomes/test/profiles.txt")
    //profiles.foreach{case (ngram,profile) => f.write(ngram+" "+profile+"\n")}
    //f.close()

  }

  def main(args:Array[String]) {
    println("running Genome ...")
    experiment2()
    println("finished.")
  }

}
