package exercises.chapterFive

import exercises.chapterFive.Stream.cons

object Exercise5_1 {

  def toList[A](stream: Stream[A]): List[A] = {
    stream match {
      case Empty => Nil
      case Cons(h, t) => h() :: toList(t())
    }
  }

}

object Exercise5_2 {

  @annotation.tailrec
  def drop[A](stream: Stream[A], n: Int): Stream[A] = {
    if (n == 0) {
      stream
    } else {
      stream match {
        case Cons(h, t) => drop(t(), n - 1)
        case Empty => Empty
      }
    }
  }

  def take[A](stream: Stream[A], n: Int): Stream[A] = {
    if (n == 0) {
      Empty
    } else {
      stream match {
        case Cons(h, t) => cons(h(), take(t(), n - 1))
        case Empty => Empty
      }
    }
  }

}

object Exercise5_3 {

  import exercises.chapterFive.Stream.cons

  def takeWhile[A](stream: Stream[A])(f: A => Boolean): Stream[A] = {
    stream match {
      case Empty => Empty
      case Cons(h, t) => if (f(h())) cons(h(), takeWhile(t())(f)) else Empty
    }
  }

}

object Exercise5_4 {

  def forAll[A](stream: Stream[A], f: A => Boolean): Boolean = {
    stream.foldRigth(true)(f(_) && _)
  }

}

object Exercise5_5 {

  def takeWhile[A](stream: Stream[A], f: A => Boolean): Stream[A] = {
    stream.foldRigth(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else Empty)
  }

}

object Exercise5_6 {

  def headOption[A](stream: Stream[A]): Option[A] = {
    stream.foldRigth(Option.empty[A])((a, b) => Some(a))
  }

  object Exercise5_7 {

    import exercises.chapterFive.Stream.{ concat, cons }

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = {
      stream.foldRigth(Empty: Stream[B])((a, b) => cons(f(a), b))
    }

    def filter[A](stream: Stream[A])(f: A => Boolean): Stream[A] = {
      stream.foldRigth(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)
    }

    def append[AA](stream: Stream[AA]): Stream[AA] = {
      stream.foldRigth(stream)((a, b) => cons(a, b))
    }

    def flatMap[A, B](stream: Stream[A])(f: A => Stream[B]): Stream[B] = {
      stream.foldRigth(Empty: Stream[B])((a, b) => concat(f(a), b))
    }

  }

  object Exercise5_8 {

    import exercises.chapterFive.Stream.cons

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

  }

  object Exercise5_9 {

    import exercises.chapterFive.Stream.cons

    def ones: Stream[Int] = cons(1, ones)

    def from(n: Int): Stream[Int] = ones.map(_ + n)

  }

  object Exercise5_10 {

    /**
     * n = 0  : 0, 1, 1, 2, 3, 5, 8
     * n = 1  : 1, 1, 2, 3, 5, 8
     * n = 2  : 1, 2, 3, 5, 8, 13
     */

    import exercises.chapterFive.Stream.cons

    def fibs: Stream[Int] = cons(0, cons(1, fibs.zipWith(fibs.drop(1)).map(pair => pair._1 + pair._2)))

  }

  object Exercise5_11 {

    import exercises.chapterFive.Stream.cons

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case None => Empty
        case Some((a, s)) => cons(a, unfold(s)(f))
      }
    }

  }

  object Exercise5_12 {

    import exercises.chapterFive.Stream.cons

    def fibs: Stream[Int] = cons(0, cons(1, Exercise5_11.unfold((0, 1))(pair => Some((pair._1 + pair._2, (pair._2, pair._1 + pair._2))))))

    def from(n: Int): Stream[Int] = Exercise5_11.unfold(n - 1)(num => Some((num + 1, num + 1)))

    def constant(n: Int): Stream[Int] = Exercise5_11.unfold(n)(num => Some((num, num)))

    def ones: Stream[Int] = Exercise5_11.unfold(1)(num => Some((num, num)))

  }

  object Exercise5_13 {

    import Exercise5_11.unfold

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = unfold(stream)({
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    })

    def takeWhile[A, B](stream: Stream[A])(f: A => Boolean): Stream[A] = unfold(stream)({
      case Empty => None
      case Cons(h, t) => if (f(h())) Some((h(), t())) else None
    })

    def zipWith[A](stream1: Stream[A], stream2: Stream[A]): Stream[(A, A)] = unfold((stream1, stream2))({
      st =>
        (st._1, st._2) match {
          case (_, Empty) => None
          case (Empty, _) => None
          case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
        }
    })

    def zipAll[A](stream1: Stream[A], stream2: Stream[A]): Stream[(Option[A], Option[A])] = unfold((stream1, stream2))({
      st =>
        (st._1, st._2) match {
          case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
          case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
          case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
          case (Empty, Empty) => None
        }
    })

  }

  object Exercise5_14 {

    def startsWith[A](parentStream: Stream[A], childStream: Stream[A]): Boolean = {
      parentStream.zipAll(childStream).takeWhile({ case (_, x) => x.isDefined }).forAll({ case (a, b) => a == b })
    }

  }

  object Exercise5_15 {

    import Exercise5_11.unfold

    def tail[A](stream: Stream[A]): Stream[Stream[A]] = {
      unfold((stream, 0))({
        case (Empty, 1) => None
        case (Empty, 0) => Option((Stream(), (Empty, 1)))
        case (other, _) => Option((other, (other.drop(1), 0)))
      })
    }

  }

  object Exercise5_16 {

    import Exercise5_15.tail
    import Stream.cons

    def scanRight[A](stream: Stream[A])(z: A)(f: (A, A) => A): Stream[A] = {
      tail(stream).foldRigth(Stream(z))({
        (a, b) =>
          (a, b) match {
            case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(), h2()), Cons(h2, t2))
            case (Empty, _) => Stream(z)
            case others => throw new Exception("Shouldn't have entered here")
          }
      })
    }

  }

}

