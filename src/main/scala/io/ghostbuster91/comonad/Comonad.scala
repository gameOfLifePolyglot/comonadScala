package io.ghostbuster91.comonad

import cats.Functor

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def join[A](ffa: F[F[A]]): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B])(implicit F: Functor[F]): F[B] =
    join(F.map(fa)(f))
}

trait Comonad[F[_]] {
  def counit[A](fa: F[A]): A
  def cojoin[A](fa: F[A]): F[F[A]]

  def coflatMap[A, B](fa: F[A])(f: F[A] => B)(implicit F: Functor[F]): F[B] =
    F.map(cojoin(fa))(f)
}

object ComonadSyntax {
  implicit class Ops[F[_], A](fa: F[A])(implicit cm: Comonad[F]) {
    def counit: A = cm.counit(fa)
    def cojoin: F[F[A]] = cm.cojoin(fa)
    def coflatMap[B](f: F[A] => B)(implicit F: Functor[F]): F[B] =
      cm.coflatMap(fa)(f)
  }
}
