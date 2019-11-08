package au.edu.imb

import org.scalatest.FunSuite

/**
 * Unit tests for sequence similarity measures
 */
class SimilarityTest extends FunSuite  {

  test("manhattan") {
    val s1 = MolecularSequence("AAAAAAAAAA", DNA)
    val s2 = MolecularSequence("AAAAAAAACC", DNA)
    expect(0.8)(Similarity.hamming(s1,s2))
  }
}