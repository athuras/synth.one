package synth.interpolation

import spire.algebra.Field
import spire.algebra.Order

import java.util.Map.Entry
import java.util.TreeMap


private final class LinearImpl[T](
  protected val points: TreeMap[T, T]
)(implicit
  order: Order[T],
  f: Field[T]
) extends Interpolant[T] {

  private[this] lazy val minKey: T = points.firstKey()
  private[this] lazy val maxKey: T = points.lastKey()

  override def isDefinedAt(x: T) =
    order.lteqv(minKey, x) && order.lteqv(x, maxKey)

  override def apply(x: T): T = {
    if (points.containsKey(x)) {
      points.get(x)
    } else {
      val result = for {
        left  <- Option(points.floorEntry(x))
        right <- Option(points.ceilingEntry(x))
      } yield interpolate(
        x1 = left.getKey,
        y1 = left.getValue,
        x2 = right.getKey,
        y2 = right.getValue,
        x = x
      )

      result match {
        case Some(y) => y
        case _ =>
          throw new IllegalArgumentException(
            s"Interpolant is not defined for point $x"
          )
      }
    }
  }

  private def interpolate(x1: T, y1: T, x2: T, y2: T, x: T): T =
    f.plus(
      y1,
      f.times(
        f.minus(y2, y1),
        f.div(
          f.minus(x, x1),
          f.minus(x1, x2)
        )
      )
    )
}


object Linear {
  def apply[T : Order : Field](points: Map[T, T]): Interpolant[T] = {
    // for some reason I need to jump through this hoop...
    val ordering = Order.catsKernelOrderingForOrder(implicitly[Order[T]])
    val tm = new TreeMap[T, T](ordering)
    points.foreach { case (k, v) =>
      tm.put(k, v)
    }
    new LinearImpl(tm)
  }
}
