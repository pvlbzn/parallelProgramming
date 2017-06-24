package util

package object Timer {
  def measure [R] (block: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val res = block
    val t1 = System.nanoTime()
    (res, (t1-t0)/1e6d)         // 10^6
  }
}
