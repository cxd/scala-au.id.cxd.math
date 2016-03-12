package au.id.cxd.math.definitions

/**
  * Definition of monad pattern to allow composition of operations
  *
  * Created by cd on 21/11/2015.
  */
trait Monad[F[_]] {

  /**
    * the identity operation
    * id a = F[A]
    * @param a
    * @tparam A
    * @return
    */
  def unit[A](a: => A): F[A]

  /**
    * flat map operation traversing from F[A] to F[B] via f(a => F[B])
    * @param ma
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def flatMap[A,B](ma:F[A])(f: A => F[B]): F[B]

  /**
    * map operation from F[A] to F[B] through f(a => b)
    * @param ma
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A,B](ma:F[A])(f:A => B):F[B] =
    flatMap(ma)(a => unit(f(a)))


  /**
    * lift a function that takes two parameters and map it via
    * F[A] F[B] => F[C] via f:(A,B) => C
    * @param ma
    * @param mb
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A,B,C](ma:F[A], mb:F[B])(f:(A,B) => C):F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

}
