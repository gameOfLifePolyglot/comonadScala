package io.ghostbuster91.comonad

object Main {
  def main(args: Array[String]): Unit = {
    val initial = StreamZipper(LazyList(1, 2, 3), 4, LazyList(5, 6, 7))
    println(initial.focus)
    println(initial.cojoin.left.head.focus)
    println(initial.cojoin.left.head.left.toList)
    println(initial.cojoin.left.head.right.toList)

    println(initial.cojoin.right.head.focus)
    println(initial.cojoin.right.head.left.toList)
    println(initial.cojoin.right.head.right.toList)
    import StreamZipper._
    import ComonadSyntax._
    println(
      StreamZipper(List(1, 2, 3), 4, List(5, 6, 7))
        .coflatMap(avg)
        .toList
    )
  }

  def avg(a: StreamZipper[Int]): Double = {
    val left = a.moveLeft.focus
    val current = a.focus
    val right = a.moveRight.focus
    (left + current + right) / 3d
  }
}
