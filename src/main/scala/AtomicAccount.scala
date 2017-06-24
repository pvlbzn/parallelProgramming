object RunAtomicAccount extends App {
  private var uidCount = 0L
  private val x = new AnyRef

  def getUniqueId(): Long = {
    uidCount = uidCount + 1
    uidCount
  }

  def atomicTransfer(a: AtomicAccount, b: AtomicAccount, n: Int): Thread = {
    val t = new Thread {
      override def run(): Unit = for (i <- 0 until n) a.transfer(b, 1)
    }
    t.start()
    t
  }

  class AtomicAccount(private var amout: Int = 0) {
    val uid = getUniqueId()

    private def lockTransfer(target: AtomicAccount, n: Int) =
      this.synchronized {
        target.synchronized {
          this.amout   -= n
          target.amout += n
        }
      }

    def transfer(target: AtomicAccount, n: Int) =
      if (this.uid < target.uid)
        this.lockTransfer(target, n)
      else
        target.lockTransfer(this, -n)

    def readAmount(): Int = this.amout
  }


  val a = new AtomicAccount(500)      // Atomic account with $500
  val b = new AtomicAccount(800)      // Atomic account with $800

  val t1 = atomicTransfer(a, b, 100)  // Transfer $100 from account a to b
  val t2 = atomicTransfer(b, a, 280)  // Transfer $280 from account b to a

  t1.join()
  t2.join()

  println(a.readAmount)               // Expected to be 680
  println(b.readAmount)               // Expected to be 620
}