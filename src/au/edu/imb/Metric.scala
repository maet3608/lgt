package au.edu.imb

import math.{abs,sqrt}

/**
 * Performance metrics for predictors
 */
object Metric {

  /**
   * Computes the Area under the ROC curve (AUC).
   * @param predictions Sequence of predicted values (e.g. confidence values of the predictor)
   * @param targets Target values as booleans
   * @return Returns AUC [0.5,1.0]
   */
  def AUC(predictions:Seq[Double], targets:Seq[Boolean]):Double = {
    val np = targets.count(identity)
    val nn = targets.size-np
    var tp,fp,otp,ofp = 0
    var oldP = Double.NaN
    var auc = 0.0
    val outputs = (predictions zip targets).sortBy{case (p,t) => p}
    for((p,t) <- outputs) {
      if(oldP != p) {
        auc += abs(fp-ofp)*(tp+otp)/2.0
        otp = tp
        ofp = fp
        oldP = p
      }
      if(t) tp+=1 else fp+=1
    }
    auc = (auc + abs(nn-ofp)*(np+otp)/2.0)/(np*nn)
    if (auc < 0.5) 1.0-auc else auc
  }

  /**
   * Computes Matthew's correlation coefficient
   * http://en.wikipedia.org/wiki/Matthews_correlation_coefficient
   * @param predictions Sequence of predicted values (e.g. confidence values of the predictor)
   * @param targets Target values as booleans
   * @param threshold Threshold to distinguish positive and negative predictions
   * @return Returns MCC [0.0,1.0]
   */
  def MCC(predictions:Seq[Double], targets:Seq[Boolean], threshold:Double):Double = {
    val pairs = predictions.map(_ > threshold) zip targets
    val tp = pairs.count{case(p,t) => t==true  && p==true}.toDouble
    val tn = pairs.count{case(p,t) => t==false && p==false}.toDouble
    val fp = pairs.count{case(p,t) => t==false && p==true}.toDouble
    val fn = pairs.count{case(p,t) => t==true  && p==false}.toDouble
    val num = tp*tn - fp*fn
    val den = sqrt((tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))
    if(den > 0.0) num/den else 0.0
  }

  /** Computes an optimized MCC, where the best threshold is chosen automatically */
  def  MCC(predictions:Seq[Double], targets:Seq[Boolean]):Double =
    predictions.toSet[Double].map(MCC(predictions,targets,_)).max

}


/** Usage example */
object MetricExample extends App {
  val predictions = Seq(0.5 , 0.4,  0.1,   0.1,   0.2)
  val targets     = Seq(true, true, false, false, true)
  println(Metric.AUC(predictions,targets))
  println(Metric.MCC(predictions,targets,0.15))
  println(Metric.MCC(predictions,targets))
}
