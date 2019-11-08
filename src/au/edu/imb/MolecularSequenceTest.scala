package au.edu.imb

import org.scalatest.FunSuite

/**
 * Unit tests for molecular sequences
 */
class MolecularSequenceTest extends FunSuite  {

  test("constructor") {
    expect("ACTG")(MolecularSequence("ACTG", DNA).toString)
  }

  test("length") {
    expect(8)(MolecularSequence("ACTGACTG", DNA).length)
  }

  test("slice") {
    val seq = MolecularSequence("ACTG", DNA)
    expect("AC")(seq(0,2).toString)
    expect("TG")(seq(2,4).toString)
  }

  test("index") {
    val seq = MolecularSequence("ACTG", DNA)
    expect('A')(seq(0))
    expect('G')(seq(3))
  }

  test("+") {
    expect("ACTGAACC")((MolecularSequence("ACTG", DNA)+"AACC").toString)
    expect("ACTGAACC")((MolecularSequence("ACTG",DNA)+MolecularSequence("AACC",DNA)).toString)
  }
}
