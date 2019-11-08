package au.edu.imb

import Metric.{AUC,MCC}
import java.io.{File, FileWriter, BufferedWriter}

/**
 * Evaluation of LGT predictors
 */
object Evaluator extends App {

  /** returns file writer */
  def createFile(filepath:String) =
    new BufferedWriter(new FileWriter(new File(filepath)))

  /** linearly spaced vector with n between start and end */
  def linspace(start:Double, end:Double, n:Int) = {
    val range = end-start
    for(i <- 0 until n) yield start+i*range/n
  }

  /** Length of the path from root to dest node */
//  def age(root:SequenceNode, destName:String) =
//    root.path(root.name2node(destName)).map(_.length).sum

  def age(root:SequenceNode, destName:String) = {
    val destNode = root.name2node(destName)
    def len(node:SequenceNode) = destNode.path(node).map(_.length).sum
    val leaves = destNode.leaves
    leaves.map(len).sum/leaves.length
  }

//  def age(root:SequenceNode, destName:String) = {
//    root.name2node(destName).leaves.length.toDouble
//  }

  val seqLengths = List(3000) //List(1000,3000,5000,7000,9000)
  val lgtLengths = List(100) // List(50,100,200,150,200)
  val treeSizes  = List(10) //List(10,20,30,40,50)
  val predictors = List(
    new LGTPredictorControl,
    //new LGTPredictorAlign("max","hm",100),
    new LGTPredictorAlign("max","eu",100),
    //new LGTPredictorAlign("max","hm",50),
    //new LGTPredictorAlign("max","eu",50),
    //new LGTPredictorAlign("std","hm",50),
    //new LGTPredictorAlign("std","eu",50),
    //new LGTPredictorAlign("std","ff",100),
    //new LGTPredictorAlign("std","lc",100),
    //new LGTPredictorAlign2("std",100),
    //new LGTPredictorFFT("std",100),
    //new LGTPredictorFFT("max",100),
    //new LGTPredictorAlign("std","ng8",100),
    //new LGTPredictorAlign("std","ld",100),
    //new LGTPredictorAlign("std","eu",100),
    //new LGTPredictorNGram2("max",8,100),
    //new LGTPredictorLCS("std",10),
    //new LGTPredictorLCS("std",100),
    //new LGTPredictorLCS("max"),
    //new LGTPredictorNGram("max",10,50),
    //new LGTPredictorNGram("max",10,100),
    new LGTPredictorNGram("max",8,50),
    new LGTPredictorNGram("max",8,100)
  )
//  val predictors = List(new LGTPredictorControl, new LGTPredictorAlign("std","hm",100)):::
//    (for(n <- 2 to 14) yield (new LGTPredictorNGram("",n,100))).toList:::
//    (for(n <- 2 to 14) yield (new LGTPredictorAlign("std","ng"+n,100))).toList

//    List(
//    new LGTPredictorSimple(true),
//    new LGTPredictorNGramStochastic(10),
//    new LGTPredictorNGram(9,10),
//    new LGTPredictorNGram(12,100),
//    new LGTPredictorControl)

  println("running...")
  val results = createFile("results.csv")
  results.write("SEQ_LEN,LGT_LEN,TREE_SIZE,METHOD,AUC,MCC,T_SRC,T_DEST,T_LGT\n")

  for(i <- 0 to 20) {
    println(i)
    for(seqLength <- seqLengths; rootSeq = MolecularSequence(seqLength,DNA);
        treeSize  <- treeSizes;  baseTree = TreeFactory.KuhnerFelsenstein(treeSize);
        lgtLength <- lgtLengths; event = LGTEvent(baseTree, new LGTActionOverwrite(lgtLength))) {
      val evolver = new LGTEvolver(Kimura(_),event)
      val seqTree = evolver.evolve(rootSeq,baseTree)
      val tSrc = age(seqTree,event.srcName)
      val tDest = age(seqTree,event.destName)
      val tLGT  = (tSrc+tDest)/2.0
      for (predictor <- predictors) {
        val auc = predictor.performance(seqTree,event,AUC)
        val mcc = predictor.performance(seqTree,event,MCC)
        results.write(List(seqLength,lgtLength,treeSize,predictor,auc,mcc,tSrc,tDest,tLGT).mkString("",",","\n"))
        results.flush()
      }
    }
  }

  results.close()
  println("finished.")
}
