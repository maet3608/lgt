package au.edu.imb

/**
 * Unit tests for alphabet class
 */

import org.scalatest.FunSuite


class AlphabetTest extends FunSuite  {

 object Nucleotides extends Alphabet("ACTG")

  test("constructor") {
    expect("A,C,T,G")(Nucleotides.toString)
  }

  test("length") {
    expect(4)(Nucleotides.length)
  }

  test("contains") {
    expect(true)(Nucleotides.contains('A'))
    expect(true)(Nucleotides.contains('C'))
    expect(true)(Nucleotides.contains('T'))
    expect(true)(Nucleotides.contains('G'))
    expect(false)(Nucleotides.contains('X'))
  }

  test("toIndex") {
    expect(0)(Nucleotides.toIndex('A'))
    expect(1)(Nucleotides.toIndex('C'))
    expect(2)(Nucleotides.toIndex('T'))
    expect(3)(Nucleotides.toIndex('G'))
  }

  test("random") {
    val nucs = (0 to 4000) map (i => Nucleotides.random)
    val counts = nucs.groupBy(identity).values.map(_.length)
    expect(true)(counts.min > 900)
    expect(true)(counts.max < 1100)
  }
}
