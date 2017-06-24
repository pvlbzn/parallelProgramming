import util.Timer

object SequentialPNorm extends App {

  def pNorm(a: Array[Int], p: Double): Int = {

      def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
        var i = s
        var sum = 0

        while (i < t) {
          sum += power(a(i), p)
          i += 1
        }

        sum
      }

    def power(n: Int, p: Double): Int = {
      math.exp(p * math.log(abs(n))).toInt
    }

    def abs(n: Int): Int = {
      n match {
        case n if n >= 0 =>  n
        case n if n <  0 => -n
      }
    }

    power(sumSegment(a, p, 0, a.length), 1/p)
  }

  val arr = for (i <- 0 to 4000000) yield i
  val (res, time) = Timer.measure(pNorm(arr.toArray, 2))

  println(s"\t$res \t$time ms")
}
