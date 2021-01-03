package io.ghostbuster91.comonad

import cats.Functor

case class StreamZipper[A](left: LazyList[A], focus: A, right: LazyList[A]) {
  def moveLeft: StreamZipper[A] =
    if (left.isEmpty) this
    else new StreamZipper[A](left.tail, left.head, focus #:: right)

  def moveRight: StreamZipper[A] =
    if (right.isEmpty) this
    else new StreamZipper[A](focus #:: left, right.head, right.tail)

  // A stream of zippers, with the focus set to each element on the left
  private lazy val lefts: LazyList[StreamZipper[A]] =
    LazyList.iterate(this)(_.moveLeft).tail.zip(left).map(_._1)

  // A stream of zippers, with the focus set to each element on the right
  private lazy val rights: LazyList[StreamZipper[A]] =
    LazyList.iterate(this)(_.moveRight).tail.zip(right).map(_._1)

  lazy val cojoin: StreamZipper[StreamZipper[A]] =
    new StreamZipper[StreamZipper[A]](lefts, this, rights)

  def map[B](f: A => B): StreamZipper[B] =
    new StreamZipper[B](left.map(f), f(focus), right.map(f))

  def toList: List[A] = left.toList.reverse ++ List(focus) ++ right.toList

  override def toString: String = {
    s"|${left.reverse} >$focus< ${right.force}|"
  }
}

object StreamZipper {
  def apply[A](left: List[A], f: A, right: List[A]): StreamZipper[A] =
    new StreamZipper[A](LazyList.from(left.reverse), f, LazyList.from(right))

  implicit object ZipperComonad extends Comonad[StreamZipper] {
    def counit[A](fa: StreamZipper[A]): A =
      fa.focus
    def cojoin[A](fa: StreamZipper[A]): StreamZipper[StreamZipper[A]] =
      fa.cojoin
  }

  implicit object ZipperFunctor extends Functor[StreamZipper] {
    override def map[A, B](fa: StreamZipper[A])(f: A => B): StreamZipper[B] =
      fa.map(f)
  }
}
