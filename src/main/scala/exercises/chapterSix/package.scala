package exercises

package object chapterSix {

  type Rand[A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def unit[A](a: A): Rand[A] = (rng: RNG) => (a, rng)

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = (rng: RNG) => {
    val (a, nextRng) = s(rng)
    (f(a), nextRng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRng) = rng.nextInt
    if (int < 0) {
      (-1 * (int + 1), nextRng)
    } else {
      (int, nextRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (noneNeg, nextRNG) = Exercise6_1.nonNegativeInt(rng)
    (noneNeg.toDouble / (Int.MaxValue.toDouble + 1.0), nextRNG)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
    val (a, nextRng1) = ra(rng)
    val (b, nextRng2) = rb(nextRng1)
    (f(a, b), nextRng2)
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

}
