package exercises.chapterTwo

object Exercise2_1 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, x: Int, y: Int): Int = {
      if (n == 0) {
        x
      } else if (n == 1) {
        y
      } else {
        go(n - 1, y, x + y)
      }
    }
    go(n, 0, 1)
  }

}

object Exercise2_2 {

  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(array: Array[A], acc: Boolean): Boolean = {
      if (!acc || array.length == 0 || array.length == 1) {
        acc
      } else {
        go(array.tail, ordered(array.head, array.tail.head) && acc)
      }
    }
    go(array, true)
  }

}

object Exercise2_3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a: A) => (b: B) => f(a, b)
  }

}

object Exercise2_4 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

}

object Exercise2_5 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}

