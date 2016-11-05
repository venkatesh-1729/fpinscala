package exercises.chapterThree

object Exercise3_1 {

  val (one: Int, two: Int, three: Int, four: Int, five: Int) = (1, 2, 3, 4, 5)
  val fortyTwo: Int = 42
  val oneHundredOne: Int = 101

  val ans = List[Int](one, two, three, four, five) match {
    case Cons(x, Cons(`two`, Cons(`four`, _))) => x
    case Nil => fortyTwo
    case Cons(x, Cons(y, Cons(`three`, Cons(`four`, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => oneHundredOne
  }

}

object Exercise3_2 {

  def tail[A](vals: List[A]): List[A] = {
    vals match {
      case Nil => throw new Exception("Tail of a empty list.")
      case Cons(h, t) => t
    }
  }

}

object Exercise3_3 {

  def setHead[A](vals: List[A])(head: A): List[A] = {
    vals match {
      case Nil => Cons(head, Nil)
      case Cons(a, b) => Cons(head, b)
    }
  }

}

object Exercise3_4 {

  @annotation.tailrec
  def drop[A](vals: List[A], n: Int): List[A] = {
    if (n == 0 || n < 0) {
      vals
    } else {
      vals match {
        case Nil => Nil
        case Cons(a, b) => drop(b, n - 1)
      }
    }
  }

}

object Exercise3_5 {

  def dropWhile[A](vals: List[A])(f: A => Boolean): List[A] = {
    vals match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else Cons(h, t)
    }
  }

}

object Exercise3_6 {

  def init[A](vals: List[A]): List[A] = {
    vals match {
      case Nil => throw new Throwable("init of empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

}

object Exercise3_7 {

}

object Exercise3_8 {

  val ans: List[Int] = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  assert(ans == List(1, 2, 3))

}

object Exercise3_9 {

  def length[A](vals: List[A]): Int = List.foldRight(vals, 0)((a, b) => b + 1)

}

object Exercise3_10 {

  def foldLeft[A, B](vals: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(vals: List[A], acc: B): B = {
      vals match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    }
    go(vals, z)
  }

}

object Exercise3_11 {

  def sum(vals: List[Int]): Int = Exercise3_10.foldLeft[Int, Int](vals, 0)(_ + _)

  def product(vals: List[Int]): Int = Exercise3_10.foldLeft[Int, Int](vals, 1)(_ * _)

}

object Exercise3_12 {

  def reverse[A](vals: List[A]): List[A] = Exercise3_10.foldLeft[A, List[A]](vals, Nil)((lis: List[A], x: A) => Cons(x, lis))

}

object Exercise3_14 {

  // append single element to a list
  def appendElement[A](vals: List[A])(a: A): List[A] = List.foldRight(vals, Cons(a, Nil))(Cons(_, _))

  // append list to another list
  def appendList[A](vals1: List[A])(vals2: List[A]): List[A] = List.foldRight(vals1, vals2)(Cons(_, _))

}

object Exercise3_15 {

  def concat[A](vals: List[List[A]]): List[A] = List.foldRight(vals, Nil: List[A])((a, b) => Exercise3_14.appendList(a)(b))

}

object Exercise3_16 {

  def addOne(vals: List[Int]): List[Int] = {
    vals match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }
  }

}

object Exercise3_17 {

  def doubleToString(vals: List[Double]): List[String] = {
    vals match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }
  }

}

object Exercise3_18 {

  def map[A, B](vals: List[A])(f: A => B): List[B] = {
    vals match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

}

object Exercise3_19 {

  def filter[A](vals: List[A])(f: A => Boolean): List[A] = {
    vals match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
    }
  }

}

object Exercise3_20 {

  def flatMap[A, B](vals: List[A])(f: A => List[B]): List[B] = Exercise3_15.concat(Exercise3_18.map(vals)(f))

}

object Exercise3_21 {

  def filter[A](vals: List[A])(f: A => Boolean): List[A] = Exercise3_20.flatMap(vals)((a: A) => if (f(a)) List(a) else Nil)

}

object Exercise3_22 {

  def addLists(vals1: List[Int], vals2: List[Int]): List[Int] = {
    (vals1, vals2) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }
  }

}

object Exercise3_23 {

  def zipWith[A](vals1: List[A], vals2: List[A])(f: (A, A) => A): List[A] = {
    (vals1, vals2) match {
      case (Nil, _) | (_, Nil) => Nil: List[A]
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

}

object Exercise3_24 {

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, Cons(_, _)) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2) {
          hasSubsequence(t1, t2)
        } else {
          hasSubsequence(t1, Cons(h2, t2))
        }
    }
  }

}

object Exercise3_25 {

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(value) => 1
      case Branch(leftTree, rightTree) => size(leftTree) + size(rightTree)
    }
  }

}

object Exercise3_26 {

  def maximum[A <: Ordered[A]](tree: Tree[A]): A = {
    tree match {
      case Leaf(value) => value
      case Branch(leftTree, rightTree) =>
        val max1: A = maximum(leftTree)
        val max2: A = maximum(rightTree)
        if (max1 <= max2) {
          max2
        } else {
          max1
        }
    }
  }

}

object Exercise3_27 {

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(value) => 1
      case Branch(leftTree, rightTree) => 1 + scala.math.max(depth(leftTree), depth(rightTree))
    }
  }

}

object Exercise3_28 {

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(leftTree, rightTree) => Branch(map(leftTree)(f), map(rightTree)(f))
    }
  }

}

object Exercise3_29 {

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(value) => f(value)
      case Branch(leftTree, rightTree) => g(fold(leftTree)(f)(g), fold(rightTree)(f)(g))
    }
  }

  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _)

  def maximum[A <: Ordered[A]](tree: Tree[A]): A = fold(tree)(identity)((a, b) => if (a <= b) a else b)

  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ max _)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])((a, b) => Branch(a, b))

}
