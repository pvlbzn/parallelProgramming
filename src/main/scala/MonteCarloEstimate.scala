import util.Parallel.parallel
import util.Timer.measure
import scala.util.Random


object MonteCarloEstimate extends App {
  def mcCount(iter: Int): Int = {
    val rX = new Random
    val rY = new Random
    var hits = 0

    for (i <- 0 until iter) {
      val x = rX.nextDouble
      val y = rY.nextDouble
      if (x*x + y*y < 1)
        hits = hits + 1
    }
    hits
  }

  def mcSeq(iter: Int): Double = {
    4.0 * mcCount(iter) / iter
  }

  def mcSeqParallel(iter: Int): Double = {
    val ((π1, π2), (π3, π4)) = parallel(
      parallel(mcCount(iter/4), mcCount(iter/4)),
      parallel(mcCount(iter/4), mcCount(iter - 3*(iter/4)))
    )
    4.0 * (π1 + π2 + π3 + π4) / iter
  }

  println(measure(mcSeq(100000000)))
  println(measure(mcSeq(100000000)))
}
