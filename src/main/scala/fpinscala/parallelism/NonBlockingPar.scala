package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

import scalaz.concurrent.Strategy

object NonBlockingPar {
  sealed trait Future[A] {
    private[parallelism] def apply(cb: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es){a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    _ => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] = es =>
    new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  private def eval[A](es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call(): Unit = r
    })

  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = es =>
    new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = scalaz.concurrent.Actor[Either[A, B]] {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }(Strategy.Executor(es))

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft((es: ExecutorService) => unit(List.empty: List[A])(es): Future[List[A]])((acc, a) => map2(acc, a)((as, b) => as :+ b))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val pfas = ps.map(asyncF(f))
    sequence(pfas)
  }

  // In order to implement choiceN I had already identified I required a function
  // with the functionality flatMapping provides
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => {
    new Future[B] {
      def apply(cb: B => Unit): Unit = f(run(es)(pa))(es)(cb)
    }
  }

  // 7.11 Define a function choiceN to select a Par based on the result of a Par[Int]
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(n => choices(n))

  def choice[A](bool: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(bool)(b => if (b) 0 else 1))(List(t, f))

  // 7.12 Define choiceN using a Map instead of a list
  def choiceNMap[K,V](k: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(k)(k => choices(k))

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    flatMap(pa)(choices)
  }

  def choiceChooser[A](n: Par[Boolean])(t: Par[A], f: Par[A]) =
    chooser(n)(if (_) t else f)

  def choiceNChooser[A](n: Par[Int])(choices: List[Par[A]]) =
    chooser(n)(choices(_))

  def choiceMapChooser[K,V](n: Par[K])(choices: Map[K,Par[V]]) =
    chooser(n)(choices(_))

  // 7.13 Define a function that runs the inner computation and returns
  // 7.14 a) My initial implementation of join was with flatMap
  def join[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(pa => pa)

  // 7.14 b) Implement flatMap with join
  def flatMapByJoin[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

}
