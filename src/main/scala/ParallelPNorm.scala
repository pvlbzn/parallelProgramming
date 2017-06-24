import util.{Parallel, Timer}

/**
  * Compute p-norm in parallel.
  */
object ParallelPNorm extends App {

  def pNorm(a: Array[Int], p: Double): Int = {

    /**
      * Parallel computation depends on threshold value. This value
      * represents length of chunks to which array will be divided.
      *
      * Too low value will blow memory with thread allocations. Too high
      * will have no difference or perform worse than SequentialPNorm.
      *
      * Correct value for threshold depends on two factors:
      *   - size of n
      *   - number of cores
      *
      * For example 4 CPU machine, n = 4 000 000, threshold = 1 000 000
      * Such that task will be nicely parallelized with good balance
      * between thread allocation overhead and parallelization itself.
      */
    val threshold = 1e6d

    def pNormRec(a: Array[Int], p: Double): Int = {
      power(segmentRec(a, p, 0, a.length), 1/p)
    }

    def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
      if (t - s < threshold)
        sumSegment(a, p, s, t)
      else {
        val m = s + (t - s) / 2
        val (s1, s2) = Parallel.parallel(
          segmentRec(a, p, s, m),
          segmentRec(a, p, m, t)
        )

        s1 + s2
      }
    }

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

    pNormRec(a, p)
  }

  val arr = for (i <- 0 to 4000000) yield i
  val (res, time) = Timer.measure(pNorm(arr.toArray, 2))

  println(s"\t$res \t$time ms")
}

