package exercises.chapterFour

object Exercise4_2 {

  def variance(xs: Seq[Double]): Option[Double] = {

    def computeMean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) {
        None
      } else {
        Some(xs.sum / xs.length)
      }
    }

    def computeVariance(xs: Seq[Double], mean: Option[Double]): Option[Double] =
      mean.flatMap(m => computeMean(xs.map(value => scala.math.pow(value - m, 2))))

    computeVariance(xs, computeMean(xs))

  }

}

object Exercise4_3 {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

}

object Exercise4_4 {

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((a: Option[A], b: Option[List[A]]) => Exercise4_3.map2(a, b)(_ :: _))

}

object Exercise4_5 {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((a: A, b: Option[List[B]]) => Exercise4_3.map2(f(a), b)(_ :: _))

}

object Exercise4_6 {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

    def getOrElse[B >: A](default: => B): B

    def orElse[EE >: E, B >: A](default: => Either[EE, B]): Either[EE, B]

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E, Nothing](value: E) extends Either[E, Nothing] {
    def map[B](f: Nothing => B): Either[E, B] = Left(value)

    def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = Left(value)

    def getOrElse[B >: Nothing](default: => B): B = default

    def orElse[EE >: E, B >: Nothing](default: => Either[EE, B]): Either[EE, B] = default

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = Left(value)
  }

  case class Right[Nothing, +A](value: A) extends Either[Nothing, A] {
    def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

    def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)

    def getOrElse[B >: A](default: => B): B = value

    def orElse[EE >: Nothing, B >: A](default: => Either[EE, B]): Either[Nothing, B] = Right(value)

    def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(f(value, _))
  }

  object Either {
    def wrapTry[A](a: => A): Either[Exception, A] = try Right(a) catch {
      case e: Exception => Left(e)
    }
  }

}

object Exercise4_7 {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((x, y) => f(x).map2(y)(_ :: _))

}

