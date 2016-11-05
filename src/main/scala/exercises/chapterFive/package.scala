package exercises

package object chapterFive {

  sealed trait Stream[+A] {

    import Stream.cons
    import Stream.concat

    def toList: List[A] = {
      this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
      }
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = {
      if (n == 0) {
        this
      } else {
        this match {
          case Empty => Empty
          case Cons(h, t) => t().drop(n - 1)
        }
      }
    }

    def take(n: Int): Stream[A] = {
      if (n == 0) {
        Empty
      } else {
        this match {
          case Empty => Empty
          case Cons(h, t) => cons(h(), t().take(n - 1))
        }
      }
    }

    def takeWhile(f: A => Boolean): Stream[A] = {
      this match {
        case Empty => Empty
        case Cons(h, t) => if (f(h())) cons(h(), t().takeWhile(f)) else Empty
      }
    }

    def foldRigth[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Empty => z
        case Cons(h, t) => f(h(), t().foldRigth(z)(f))
      }
    }

    def exists(f: A => Boolean): Boolean = {
      this.foldRigth(false)(f(_) || _)
    }

    def zipWith[AA >: A](another: Stream[AA]): Stream[(AA, AA)] = {
      (this, another) match {
        case (_, Empty) => Empty
        case (Empty, _) => Empty
        case (Cons(h1, t1), Cons(h2, t2)) => cons((h1(), h2()), t1().zipWith(t2()))
      }
    }

    import Stream.unfold

    def zipAll[AA >: A](another: Stream[AA]): Stream[(Option[AA], Option[AA])] = unfold((this, another))({
      st =>
        (st._1, st._2) match {
          case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
          case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
          case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
          case (Empty, Empty) => None
        }
    })

    def forAll(f: A => Boolean): Boolean = {
      this.foldRigth(true)(f(_) && _)
    }

    def takeWhileWithFoldRight(f: A => Boolean): Stream[A] = {
      this.foldRigth(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else Empty)
    }

    def headOption: Option[A] = {
      this.foldRigth(Option.empty[A])((a, b) => Some(a))
    }

    def map[B](f: A => B): Stream[B] = {
      this.foldRigth(Empty: Stream[B])((a, b) => cons(f(a), b))
    }

    def filter(f: A => Boolean): Stream[A] = {
      this.foldRigth(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)
    }

    def append[AA >: A](stream: Stream[AA]): Stream[AA] = {
      this.foldRigth(stream)((a, b) => cons(a, b))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      this.foldRigth(Empty: Stream[B])((a, b) => concat(f(a), b))
    }

    def tail: Stream[Stream[A]] = {
      unfold((this, 0))({
        case (Empty, 1) => None
        case (Empty, 0) => Option((Stream(), (Empty, 1)))
        case (other, _) => Option((other, (other.drop(1), 0)))
      })
    }

    def scanRight[AA >: A](z: AA)(f: (AA, AA) => AA): Stream[AA] = {
      this.tail.foldRigth(Stream(z))({
        (a, b) =>
          (a, b) match {
            case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(), h2()), Cons(h2, t2))
            case (Empty, _) => Stream(z)
            case others => throw new Exception("Shouldn't have entered here")
          }
      })
    }

  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  case object Empty extends Stream[Nothing]

  object Stream {

    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def concat[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] = {
      stream1 match {
        case Empty => stream2
        case Cons(h, t) => cons(h(), concat(t(), stream2))
      }
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) {
        Empty
      } else {
        cons(as.head, apply(as.tail: _*))
      }
    }

    def unfold[B, S](z: S)(f: S => Option[(B, S)]): Stream[B] = {
      f(z) match {
        case None => Empty
        case Some((a, s)) => cons(a, unfold(s)(f))
      }
    }

  }

}
