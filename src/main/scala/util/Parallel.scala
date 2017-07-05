package util

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, RecursiveTask}

import scala.util.DynamicVariable

package object Parallel {

  val forkJoinPool = new ForkJoinPool()

  abstract class TaskScheduler {
    def schedule [T] (body: => T): ForkJoinTask[T]
    def parallel [A, B] (ATask: => A, BTask: => B): (A, B) = {
      val right = task {
        BTask
      }
      val left = ATask

      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule [T] (body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      forkJoinPool.execute(t)
      t
    }
  }

  var scheduler = new DynamicVariable[TaskScheduler](
    new DefaultTaskScheduler
  )

  def task [T] (body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  /**
    * Arguments are taken by name. Signature
    *   def parallel [A, B] (ATask: A, BTask B): (A, B) = { ... }
    * will work, but not in parallel. Because this signature will
    * evaluate sequentially. For parallelism, need to pass
    * unevaluated computations, call by name.
    */
  def parallel [A, B] (ATask: => A, BTask: => B): (A, B) = {
    scheduler.value.parallel(ATask, BTask)
  }

  def parallel [A, B, C, D] (ATask: => A,
                             BTask: => B,
                             CTask: => C,
                             DTask: => D): (A, B, C, D) = {
    val at = task { ATask }
    val bt = task { BTask }
    val ct = task { CTask }
    val dt = DTask
    (at.join(), bt.join(), ct.join(), dt)
  }

}
