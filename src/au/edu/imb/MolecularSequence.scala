package au.edu.imb

import scala.math.random

/**
 * Molecular sequences, eg. nucleotide or amino acid sequences
 */
class MolecularSequence(val letters:String, val alphabet:Alphabet) extends Seq[Char] {
  letters.foreach(check)

  /** Tests whether letters is in alphabet associated with this sequence. Throws exception if not. */
  def check(letter:Char)
    { if(!alphabet.contains(letter)) throw new Exception("Invalid letter in sequence: "+letter) }

  /** Returns letter at index position (zero based) */
  def apply(idx:Int):Char = letters(idx)

  /** Creates new sub sequence from from to until, with until excluded */
  def apply(from:Int, until:Int):MolecularSequence
    = new MolecularSequence(letters.slice(from,until), alphabet)

  /** Creates new sequence by appending given sequence to this sequence */
  def +(s:MolecularSequence) = new MolecularSequence(letters++s.letters, alphabet)

  /** Creates new sequence by appending given string to this sequence */
  def +(s:String) = new MolecularSequence(letters+s, alphabet)

  /** length of the sequence */
  def length = letters.length

  /** iterator over sequence letters */
  def iterator = letters.iterator

  /** Returns string representation */
  override def toString = letters
}


/** Factory */
object MolecularSequence {

  /** Creates a sequence with the given letters */
  def apply(letters:String, alphabet:Alphabet) = new MolecularSequence(letters,alphabet)

  /** Creates a random sequence of length n */
  def apply(n:Int, alphabet:Alphabet):MolecularSequence =
    new MolecularSequence( ((0 until n).map(_ => alphabet.random)).mkString, alphabet)
}


/** Usage Example */
object MolecularSequenceExample extends App {
  val seq1 = MolecularSequence("ACTG", DNA)
  val seq2 = MolecularSequence("GTCA", DNA)
  println(seq1+seq2)
  val seq3 = MolecularSequence(30, DNA)
  println(seq3)
}