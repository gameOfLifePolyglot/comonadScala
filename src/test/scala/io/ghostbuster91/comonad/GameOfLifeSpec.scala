package io.ghostbuster91.comonad

import io.ghostbuster91._
import io.ghostbuster91.gameoflife.Coord
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class GameOfLifeSpec extends AnyFreeSpec with Matchers {

  val singleLife = Map((0, 0) -> true)

  "Should alone life die after one tick" in {
    val initial = Store[Coord, Boolean](coord =>
      (singleLife at (0, 0)).getOrElse(coord, false)
    )(0, 0)
    val after = gameoflife.step(initial)
    after.lookup((0, 0)) shouldBe false
  }

  "Should life not die after one tick when has two neighbours" in {
    val initial = Store[Coord, Boolean](coord =>
      ((singleLife at (0, 0)) ++ (singleLife at (1, 1)) ++ (singleLife at (-1, -1)))
        .getOrElse(coord, false)
    )(0, 0)
    val after = gameoflife.step(initial)
    after.lookup((0, 0)) shouldBe true
  }

  "Should life not die after one tick when has three neighbours" in {
    val initial = Store[Coord, Boolean](coord =>
      ((singleLife at (0, 0)) ++ (singleLife at (1, 1)) ++ (singleLife at (-1, -1)) ++ (singleLife at (1, 0)))
        .getOrElse(coord, false)
    )(0, 0)
    val after = gameoflife.step(initial)
    after.lookup((0, 0)) shouldBe true
  }

  "Should life die after one tick when has more then three neighbours" in {
    val initial = Store[Coord, Boolean](coord =>
      ((singleLife at (0, 0)) ++ (singleLife at (1, 1)) ++ (singleLife at (-1, -1)) ++ (singleLife at (1, 0)) ++ (singleLife at (-1, 0)))
        .getOrElse(coord, false)
    )(0, 0)
    val after = gameoflife.step(initial)
    after.lookup((0, 0)) shouldBe false
  }

  "Should dead life emerge after one tick when has exactly three neighbours" in {
    val initial = Store[Coord, Boolean](coord =>
      ((singleLife at (1, 1)) ++ (singleLife at (-1, -1)) ++ (singleLife at (1, 0)))
        .getOrElse(coord, false)
    )(0, 0)
    val after = gameoflife.step(initial)
    after.lookup((0, 0)) shouldBe true
  }

  implicit class InitOps(pairs: Map[Coord, Boolean]) {
    def at(coord: Coord): Map[Coord, Boolean] =
      pairs.map {
        case ((x, y), v) => ((x + coord._1, y + coord._2), v)
      }
  }
}
