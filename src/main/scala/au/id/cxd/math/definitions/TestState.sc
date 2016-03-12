
import State._

case class State[S, +A](run: S => (A, S)) {

  /**
    * map state of f(a) to State(s => (f(a), s))
    * @param f
    * @tparam B
    * @return
    */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  /**
    * compose two state containers
    * @param sb
    * @param f
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  /**
    * flatMap operation
    * take s and execute function f:S => (A,S)
    * then run f(a) to produce new state
    * @param f
    * @tparam B
    * @return
    */
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((s: S) => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

}

object State {

  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State((_) => ((), s))
}

import java.lang.Math._

type Limit = (Double, Double)
type Step = (Double, Limit)

/**
  * a trait for integration
  */
trait Integration {

  val prevStep:Step

  def next(): Integration

}


/**
  * Trapezoid method of calculating integral.
  * @param f
  * @param level
  */
abstract class Trapd(val f: Double => Double, val level: Int = 1) extends Integration {

  /**
    * next step using trapd algorithm
    * when the level is greater than one
    * determine the n refinements in level
    * @return
    */
  def next(): Integration = {
    val (xPrior, limit) = prevStep
    val (a, b) = limit

    val nextX = if (level == 1)
      0.5 * (b - a) * (f(a) + f(b))
    else {
      var len = 1
      for (i <- 1 to level) {
        len = len << 1
      }
      val delta = (b - a) / len
      val x = a + 0.5 * delta
      val steps = for (j <- 0 to len) yield (x + j * delta)
      val steps2 = steps map { xn => f(xn) }
      steps2 reduce (_ + _)
    }
    val result = (nextX, limit)
    new Trapd(f, level + 1) {
      val prevStep = result
    }
  }
}

object Trapd {
  def apply(f: Double => Double, level: Int)(s:Step) =
    new Trapd(f, level) {
      val prevStep = s
    }

}

def exampleNorm(mu: Double, sigma: Double)(y: Double) =
  1 / (sqrt(2 * Math.PI) * sigma) * exp(-(1 / (2 * pow(sigma, 2.0))) * pow(y - mu, 2.0))

val trapd = Trapd(exampleNorm(0, 1), 1) {
  (0, (0,1))
}


