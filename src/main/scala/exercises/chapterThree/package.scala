package exercises

package object chapterThree {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](vals: A*): List[A] = {
      if (vals.isEmpty) { Nil }
      else { Cons(vals.head, apply(vals.tail: _*)) }
    }

    def sum(vals: List[Int]): Int = {
      vals match {
        case Nil => 0
        case Cons(h, t) => h + sum(t)
      }
    }

    def foldRight[A, B](vals: List[A], z: B)(f: (A, B) => B): B = {
      vals match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }
    }
  }

  sealed trait Tree[+A]
  case class Leaf[+A](value: A) extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

}
