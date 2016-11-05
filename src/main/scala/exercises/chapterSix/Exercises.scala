package exercises.chapterSix

object Exercise6_1 {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRng) = rng.nextInt
    if (int < 0) {
      (-1 * (int + 1), nextRng)
    } else {
      (int, nextRng)
    }
  }
}

object Exercise6_2 {
  def double(rng: RNG): (Double, RNG) = {
    val (noneNeg, nextRNG) = Exercise6_1.nonNegativeInt(rng)
    (noneNeg.toDouble / (Int.MaxValue.toDouble + 1.0), nextRNG)
  }
}

object Exercise6_3 {
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, nextRng1) = rng.nextInt
    val (double, nextRng2) = Exercise6_2.double(nextRng1)
    ((int, double), nextRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (double, nextRng1) = Exercise6_2.double(rng)
    val (int, nextRng2) = nextRng1.nextInt
    ((double, int), nextRng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, nextRng1) = Exercise6_2.double(rng)
    val (double2, nextRng2) = Exercise6_2.double(nextRng1)
    val (double3, nextRng3) = Exercise6_2.double(nextRng2)
    ((double1, double2, double3), nextRng3)
  }
}

object Exercise6_4 {
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    0.until(count).foldRight((List[Int](), rng))({
      (a, state) =>
        val (nextInt, nextRng) = state._2.nextInt
        (nextInt :: state._1, nextRng)
    })
  }
}

object Exercise6_5 {
  def double(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)({ noneNeg => noneNeg.toDouble / (Int.MaxValue.toDouble + 1.0) })
  }
}

object Exercise6_6 {
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
    val (a, nextRng1) = ra(rng)
    val (b, nextRng2) = rb(nextRng1)
    (f(a, b), nextRng2)
  }
}

object Exercises6_7 {

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(_.nextInt): List[Rand[Int]])(rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = (rng: RNG) => {
    fs.foldRight((List.empty[A], rng))({
      (rand, state) =>
        val (next, nextRng) = rand(state._2)
        (next :: state._1, nextRng)
    })
  }
}

object Exercises6_8 {
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = (rng: RNG) => {
    val (value, nextRng) = f(rng)
    g(value)(nextRng)
  }
}

/*
  Reimplement map and map2 in terms of flatMap.
  The fact that this is possible is what we’re referring to when we say that flatMap is more powerful than map and map2.
 */
object Exercise6_9 {

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = Exercises6_8.flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    Exercises6_8.flatMap({
      (rng: RNG) =>
        val (a, nextRng1) = ra(rng)
        val (b, nextRng2) = rb(nextRng1)
        ((a, b), nextRng2)
    })({
      pair => unit(f(pair._1, pair._2))
    })
  }

}

/*
  Generalize the functions unit, map, map2, flatMap, and sequence.
  Add them as meth- ods on the State case class where possible. Otherwise you should put them in a State companion object.
 */

object Exercise6_10 {

  type Rand[A] = State[RNG, A]

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = State((s: S) => {
      val (a, nextS) = run(s)
      (f(a), nextS)
    })

    def map2[B, C](rb: State[S, B], f: (A, B) => C): State[S, C] = State((s: S) => {
      val (a, nextS1) = run(s)
      val (b, nextS2) = rb.run(nextS1)
      (f(a, b), nextS2)
    })

    def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
      val (a, nextS) = run(s)
      f(a).run(nextS)
    })

  }

  object State {

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(
      (s: S) => fs.foldLeft((List.empty[A], s))((pair, state) => {
        val (a, nextState) = state.run(pair._2)
        (pair._1 ++ List(a), nextState)
      })
    )

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def get[S]: State[S, S] = State(s => (s, s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }
}

object Exercise6_11 {

  sealed trait Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {

    import Exercise6_10.State

    def singleStep(input: Input)(mac: Machine): Machine = {
      input match {
        case Coin =>
          if (mac.locked && mac.candies != 0) {
            Machine(!mac.locked, mac.candies, mac.coins + 1)
          } else {
            mac
          }
        case Turn => {
          if (!mac.locked && mac.candies != 0) {
            Machine(mac.locked, mac.candies - 1, mac.coins)
          } else {
            mac
          }
        }
      }
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State((mac: Machine) => {
      /* With for comprehension
      for{
        x: List[Unit] <- State.sequence(inputs.map(inp => State.modify[Machine](singleStep(inp)) ))
        s: Machine <- State.get
      } yield (s.coins, s.candies)
    }
    */
      val ans: State[Machine, List[Unit]] = State.sequence(inputs.map(inp => State.modify[Machine](singleStep(inp))))
      val pair: (List[Unit], Machine) = ans.run(mac)
      ((pair._2.coins, pair._2.candies), pair._2)
    })

  }

  case object Coin extends Input

  case object Turn extends Input

}

