package au.edu.imb

import org.scalatest.FunSuite

/**
 * Unit tests over the entire system
 */
class AllTests extends FunSuite {
  (new AlphabetTest).execute()
  (new MolecularSequenceTest).execute()
  (new NodeTest).execute()
  (new LGTEventTest).execute()
  (new SimilarityTest).execute()
  (new TreeParserTest).execute()
  (new LGTPredictorTest).execute()
}