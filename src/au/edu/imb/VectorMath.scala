package au.edu.imb

import math.{sqrt,abs}

/**
 * Functions mainly for math on vectors.
 */
object VectorMath {
  type V = Seq[Double]

  def mean(vs:V) = vs.sum / vs.length.toDouble
  def max(vs:V) = vs.max
  def min(vs:V) = vs.min
  def std(vs:V) = {
    val m = mean(vs)
    sqrt( vs.map(v => (v-m)*(v-m)).sum / vs.length )
  }

  def diff(xs:V,ys:V) = (xs zip ys) map {case (x,y) => x-y}
  def absdiff(xs:V,ys:V) = (xs zip ys) map {case (x,y) => abs(x-y)}
}
