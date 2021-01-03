package io.ghostbuster91.comonad

import cats.Functor
import cats.implicits._

case class Store[S, A](lookup: S => A)(val index: S) {
  lazy val counit: A =
    lookup(index)

  lazy val cojoin: Store[S, Store[S, A]] =
    Store(Store(lookup))(index)

  def map[B](f: A => B): Store[S, B] =
    Store(lookup.andThen(f))(index)

  def experiment[F[_]: Functor](fn: S => F[S]): F[A] = {
    fn(index).map(lookup)
  }
}
