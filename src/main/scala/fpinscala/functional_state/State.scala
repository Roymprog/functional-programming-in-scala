package fpinscala.functional_state

// 6.10 Generalize unit,map, map2, flatmap and sequence to state
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, nextState) = run(s)
      g(a).run(nextState)
    })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = State{s =>
    val (a, s2) = run(s)
    val (b, s3) = rb.run(s2)
    (f(a, b), s3)
  }
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def map[S, A, B](s : State[S,A])(f: A => B): State[S, B] =
    s.map(f)

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    ra.map2(rb)(f)

  def flatMap[S, A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] =
    s.flatMap(f)

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldLeft(State.unit(List.empty): State[S, List[A]])(map2(_, _)((a, b) => a :+ b))
}
