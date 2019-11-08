package au.edu.imb

import math.exp
import scala.math.random


/**
 * Describes a transition probability matrix for substitutions.
 * @param name  Name of the model, e.g. Jukes-Cantor
 * @param alphabet Alphabet it operates on, e.g. DNA
 */
abstract class SubstitutionModel(val name:String, val alphabet:Alphabet) {
  /** Returns new sequence with substitutions */
  def apply(sequence:MolecularSequence):MolecularSequence
}


/**
 * Substitution models for individual substitutions, e.g. nucleotides
 * instead of codon substitutions, for instance.
 * @param name  Name of the model, e.g. Jukes-Cantor
 * @param alphabet Alphabet it operates on, e.g. DNA
 */
abstract class SingleLetterSubstitutionModel(name:String, alphabet:Alphabet)
  extends SubstitutionModel(name,alphabet) {

  /** Returns probability for a substitution from a to b. */
  def apply(a:Char, b:Char):Double

  /** Returns new sequence with substitutions */
  def apply(sequence:MolecularSequence):MolecularSequence = {
    assert(sequence.alphabet == alphabet)
    def sub(a:Char):Char = {
      val r = random
      var p = 0.0
      for(b <- alphabet) {
        p += this(a,b)
        if(p >= r) return b
      }
      return a
    }
    MolecularSequence(sequence.letters.map(sub), sequence.alphabet)
  }
}



/**
 * Jukes Cantor transition probability matrix. Performs single nucleotide substitutions.
 * formula adapted from http://en.wikipedia.org/wiki/Models_of_DNA_evolution
 * @param t branch length
 */
class JukesCantor(t:Double) extends SingleLetterSubstitutionModel("Jukes-Cantor", DNA) {
  private val lambda = 1.0/3.0
  private val o = 1.0/4
  private val e = exp(-4*lambda*t)/4
  private def p(a:Char, b:Char) = if(a==b) o+3.0*e else o-1.0*e
  private val ps = Map() ++ (for(a <- alphabet; b <- alphabet) yield ((a,b), p(a,b)) )

  def apply(a:Char, b:Char) = ps((a,b))
}


/** Factory */
object JukesCantor {
  def apply(t:Double) = new JukesCantor(t)
}

/**
 * Kimura transition probability matrix. Performs single nucleotide substitutions.
 * @param t branch length
 */
class Kimura(t:Double) extends SingleLetterSubstitutionModel("Kimura", DNA) {
  private val R = 2.0
  private val R1 = exp(-(2*R-1)/(R+1)*t)
  private val R2 = exp(-2/(R+1)*t)
  private val transition = 1.0/4 - 1.0/2*R1 + 1.0/4*R2
  private val transversion = 1.0/2 - 1.0/2*R2

  def apply(a:Char, b:Char) = (a,b) match {
    case ('T','C') => transition
    case ('C','T') => transition
    case ('A','G') => transition
    case ('G','A') => transition
    case _ => transversion
  }
}

/** Factory */
object Kimura {
  def apply(t:Double) = new Kimura(t)
}


/** Usage example */
object SubstitutionModelExample extends App {
  val seq = MolecularSequence("AAAAAAAAAAAA", DNA)
  val model = JukesCantor(0.1)
  println(model(seq))
}