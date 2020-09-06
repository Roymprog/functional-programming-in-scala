package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit}


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(b: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(l: Long, timeUnit: TimeUnit): A = get
  }

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get())

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    reduce(ints, 0)(_ + _)
  }

  // 7.1 Define generic map2 function combining two Pars
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get()))
  }

  private case class CombinedFuture[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    override def cancel(bool: Boolean): Boolean = a.cancel(bool) && b.cancel(bool)

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def isDone: Boolean = a.isDone && b.isDone

    override def get(l: Long, timeUnit: TimeUnit): C = {
      val beforeA = System.nanoTime()
      val vala = a.get(l, timeUnit)
      val afterA = System.nanoTime()
      val newTimeOut = timeUnit.toNanos(l) - (afterA - beforeA)
      val valb = b.get(newTimeOut, TimeUnit.NANOSECONDS)
      f(vala, valb)
    }

    override def get(): C = f(a.get, b.get)
  }

  // 7.3 Fix map2 so it respects timeouts on Future
  def map2Timeout[A,B,C](para: Par[A], parb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    CombinedFuture(para(es), parb(es), f)
  }

  // 7.4 Define a function that converts any function to one evaluating its result asynchronously
  def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  // 7.5 Define a function sequence converting List[Par[A]] to Par[List[A]]
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft((_: ExecutorService) => UnitFuture(List.empty): Future[List[A]])((acc, a) => map2(acc, a)((as, b) => as :+ b))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val pfas = ps.map(asyncF(f))
    sequence(pfas)
  }

  // 7.6 Define a parallel filter function
  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val psals = parMap(ps)((a: A) => if (f(a)) List(a) else List.empty)
    map(psals)(_.flatten)
  }

  def reduce[A](as: Seq[A], unit: A)(combine: (A, A) => A): Par[A] = {
    def go(as: Seq[A]): Par[A] = {
      if (as.size <= 1)
        Par.unit(as.headOption.getOrElse(unit))
      else {
        val (l, r) = as.splitAt(as.length/2)
        Par.map2(Par.fork(go(l)), Par.fork(go(r)))(combine(_, _))
      }
    }

    go(as)
  }
}
