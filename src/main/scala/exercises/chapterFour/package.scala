package exercises

package object chapterFour {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](default: => Option[B]): Option[B]
  }

  case class Some[+A](get: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = Some(f(get))
    def flatMap[B](f: A => Option[B]): Option[B] = f(get)
    def getOrElse[B >: A](default: => B): B = get
    def orElse[B >: A](default: => Option[B]): Option[B] = Some(get)
  }

  case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[B] = None
    def flatMap[B](f: Nothing => Option[B]): Option[B] = None
    def getOrElse[B >: Nothing](default: => B): B = default
    def orElse[B >: Nothing](default: => Option[B]): Option[B] = default
  }

  object Option {
    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
    def wrapTry[A](a: => A): Option[A] = try Some(a) catch {
      case e: Exception => None
    }
  }

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
