package au.edu.imb

import io.Source._

/**
 * Manages gene annotation loaded in PTT format.
 */

case class Annotation(
  start:Int,
  end:Int,
  length:Int,
  forward:Boolean,
  id:String,
  name:String,
  description:String,
  cogs:Set[String]) {
}

class Annotations(val annotations:Iterable[Annotation]) extends Iterable[Annotation] {

  // returns a profile with zeros and ones indicating gene sections on the sequence
  def geneProfile(sequence:String) = {
    val profile = Array.fill(sequence.size)(0)
    for (a <- annotations)
      (a.start to a.end).foreach(pos => profile(pos-1) = 1)
    profile
  }
  def iterator = annotations.iterator
}


object Annotations {

  /** Returns gene annotations loaded from the specified file */
  def load(path:String):Annotations = {
    val annotations = for(line <- fromFile(path).getLines().drop(3);
      val elems = line.split('\t');
      val Array(start,end) = elems(0).split("\\.\\.").map(_.toInt);
      val cogs = if(elems(7)=="-") Set[String]() else elems(7).split(',').toSet
    ) yield Annotation(start, end, elems(2).toInt, elems(1)=="+", elems(3), elems(4), elems(8), cogs)
    new Annotations(annotations.toList)
  }


  def main(args:Array[String]) {
    println("running ...")
    val annotations = load("f:/LGTEvent/genomes/test/AC_000091.ptt")
    for(a <- annotations)
      println(a)
    println("finished.")
  }

}
