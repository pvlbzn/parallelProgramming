class SimpleThread extends Thread {
  override def run(): Unit = {
    println("hi")
  }
}

val t = new SimpleThread
t.start()
t.join()


class TwoPrintThread(id: String) extends Thread {
  override def run(): Unit = {
    println(s"$id: Hello ")
    println(s"$id: World!")
  }
}

def main(): Unit = {
  val t = new TwoPrintThread("1")
  val s = new TwoPrintThread("2")

  t.start()
  s.start()
  t.join()
  s.join()
}

main()
main()
main()


/*

  Atomicity

 */

private var uidCount = 0L
private val x = new AnyRef {}

def getUniqueId(): Long = {
  uidCount = uidCount + 1
  uidCount
}
//
//def getAtomicUniqueId(): Long = x.synchronized {
//  uidCount = uidCount + 1
//  uidCount
//}
//
//def startThread() = {
//  val t = new Thread {
//    override def run(): Unit = {
//      val uids = for (i <- 0 until 10) yield getAtomicUniqueId()
//      println(uids)
//    }
//  }
//  t.start()
//  t
//}
//
//startThread() ; startThread()


class Account(private var amount: Int = 0) {
  val uid = getUniqueId()

  private def lockAndTransfer(target: Account, n: Int) =
    this.synchronized {
      target.synchronized {
        this.amount   -= n
        target.amount += n
      }
    }

  def transfer(target: Account, n: Int) =
    if (this.uid < target.uid)
      this.lockAndTransfer(target, n)
    else
      target.lockAndTransfer(this, -n)

  def getAmount(): Int = {
    this.amount
  }
}

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (i <- 0 until n) {
        a.transfer(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(500)           // 500
val a2 = new Account(800)           // 800

val t1 = startThread(a1, a2, 100)   // a1: 400, a2: 900
val s1 = startThread(a2, a1, 280)   // a1: 680, a2: 620

t1.join()
s1.join()

println(a1.getAmount())
println(a2.getAmount())


// Write a recursive function which will sum up the array
// in some range

//val arr = for (i <- 0 to 9) yield i
//
//def sum(a: List[Int]): Int = {
//  def inner(a: List[Int], acc: Int): Int = {
//    a match {
//      case x :: tail => x + inner(tail, acc + x)
//      case Nil => 0
//    }
//  }
//  inner(a, 0)
//}
//
//sum(arr.toList)


/*

  p-Norm

 */

def pNorm(a: Array[Int], p: Double): Int = {
  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    var i   = s
    var sum = 0

    while(i < t) {
      sum += power(a(i), p)
      i += 1
    }

    sum
  }

  def power(x: Int, p: Double): Int = {
    math.exp(p * math.log(abs(x))).toInt
  }

  def abs(x: Int): Int = {
    x match {
      case x if x >= 0 =>  x
      case x if x <  0 => -x
    }
  }

  power(sumSegment(a, p, 0, a.length), 1/p)
}


/*

  p-Norm parallelization

 */

def pNormRec(a: Array[Int], p: Double): Int = {
  power(segmentRec(a, p, 0, a.length), 1/p)
}

def segmentRec(a: Array[Int], p: Double, s: Int, t: Int) = {
  if (t - s < threshold)
    sumSegment(a, p, s, t)
  else {
    val m = s + (t - s) / 2
    val (s1, s2) = parallel(
      segmentRec(a, p, s, m),
      segmentRec(a, p, m, t)
    )

    s1 + s2
  }
}




