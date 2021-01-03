package io.ghostbuster91

import cats.Functor
import cats.implicits.catsStdInstancesForList
import io.ghostbuster91.comonad.ComonadSyntax._
import io.ghostbuster91.comonad.{Comonad, Store}

package object gameoflife {
  type Coord = (Int, Int)
  type Grid[A] = Store[Coord, A]

  implicit object GridFunctor extends Functor[Grid] {
    override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] = fa.map(f)
  }

  implicit object GridComonad extends Comonad[Grid] {
    override def counit[A](fa: Grid[A]): A = fa.counit

    override def cojoin[A](fa: Grid[A]): Grid[Grid[A]] = fa.cojoin
  }

  def step(grid: Grid[Boolean]): Grid[Boolean] =
    grid.coflatMap(conway)

  def conway(grid: Grid[Boolean]): Boolean = {
    val neighbours = grid.experiment {
      case (x, y) => neighbourCoords(x, y)
    }
    val liveCount = neighbours.count(identity)
    grid.counit match {
      case true if liveCount < 2                    => false
      case true if liveCount == 2 || liveCount == 3 => true
      case true if liveCount > 3                    => false
      case false if liveCount == 3                  => true
      case x                                        => x
    }
  }

  def neighbourCoords(x: Int, y: Int): List[Coord] =
    List(
      (x + 1, y),
      (x - 1, y),
      (x, y + 1),
      (x, y - 1),
      (x + 1, y + 1),
      (x + 1, y - 1),
      (x - 1, y + 1),
      (x - 1, y - 1)
    )
}
