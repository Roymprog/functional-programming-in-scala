package fpinscala.functional_state

// 6.10 Generalize unit,map, map2, flatmap and sequence to state
case class State[S, +A](run: S => (A, S)) {

  def unit: State.State[S, A] = run

  def map[B](f: A => B): State.State[S, B] = rng => {
    val (a, nextRng) = run(rng)
    (f(a), nextRng)
  }

  def flatMap[B](g: A => State.State[S, B]): State.State[S, B] = rng => {
    val (a, nextRNG) = run(rng)
    g(a)(nextRNG)
  }

  def map2[B, C](rb: State.State[S, B])(f: (A, B) => C): State.State[S, C] = rng => {
    val (a, rng1) = run(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }
}

object State {

  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def map[S, A, B](s : State[S,A])(f: A => B): State[S, B] = rng => {
    val (a, nextRng) = s(rng)
    (f(a), nextRng)
  }

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = rng => {
    val (a, nextRNG) = f(rng)
    g(a)(nextRNG)
  }

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldLeft((s: S) => (List.empty: List[A], s))(map2(_, _)((a, b) => a :+ b))
}
