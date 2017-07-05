
object SimpleThread extends App {

  class SimpleThread(private val id: Int) extends Thread {
    override def run(): Unit = {
      println(s"id: $id hello")
      println(s"id: $id world")
    }
  }
  
  val t1 = new SimpleThread(1)
  val t2 = new SimpleThread(2)

  t1.start()
  t2.start()

  t1.join()
  t2.join()
}