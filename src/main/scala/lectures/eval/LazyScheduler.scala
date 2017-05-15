package lectures.eval

import java.time.Clock

import scala.collection.SeqView

/**
  * В этом задании ваша задча реализовать своеобразный View с таймером.
  *
  * Он должен представлять из себя стандартный SeqView c ограничением по времени.
  * Т.е. этот view ведет себя как обычно, пока не истечет таймаут, предеданный при создании.
  * Как только таймаут истекает, view должен начать вести себя так, как будто коллекция пуста.
  *
  * Для решения задачи подставьте на место вопросительных знаков реализацию view.
  * Раскомментируйте и выполните тесты в lectures.eval.LazySchedulerTest
  */

object LazySchedulerView {

  implicit class SeqViewConverter[A](f: Seq[A]) {
    val c = Clock.systemDefaultZone()

    /**
      *
      * @param expirationTimeout - таймаут, после которого view становится пустым, в миллисекундах
      * @return - view
      */
    def lazySchedule(expirationTimeout: Long): SeqView[A, Seq[_]]  = {
      val i = c.instant().plusMillis(expirationTimeout)
      new SeqView[A, Seq[_]] {
        override protected def underlying: Seq[_] = if(c.instant.isBefore(i)) f else Seq()

        override def length: Int = if(c.instant.isBefore(i)) f.length else 0

        override def apply(idx: Int): A = if(c.instant.isBefore(i)) f.apply(idx) else Seq()(idx)

        override def iterator: Iterator[A] = if(c.instant.isBefore(i)) f.iterator else Seq().iterator
      }
    }
  }
}

object LazySchedulerViewExample extends App {

  import LazySchedulerView._

  val v = List(1, 2, 3, 56)
  val d = v.lazySchedule(1300)

  print(d.length)
  Thread.sleep(1500)
  print(d.length)
}
