package synth.interpolation

import cats.kernel.Order
import spire.algebra.SignedAdditiveAbGroup

import java.util.TreeMap


private final class NearestImpl[T](
  protected val points: TreeMap[T, T]
)(implicit
  group: SignedAdditiveAbGroup[T]
) extends Interpolant[T] {
  require(!points.isEmpty)

  private[this] lazy val minKey: T = points.firstKey()
  private[this] lazy val maxKey: T = points.lastKey()

  override def isDefinedAt(x: T) =
    group.lteqv(minKey, x) && group.lteqv(x, maxKey)

  override def apply(x: T): T = {
    if (points.containsKey(x)) {
      points.get(x)
    } else {
      val result = for {
        left  <- Option(points.floorEntry(x))
        right <- Option(points.ceilingEntry(x))
      } yield {
        val dl = group.abs(group.minus(x, left.getKey))
        val dr = group.abs(group.minus(x, right.getKey))
        if (group.lteqv(dl, dr)) left.getValue else right.getValue
      }

      result match {
        case Some(y) => y
        case _ =>
          throw new IllegalArgumentException(
            s"Interpolant is not defined for point $x."
          )
      }
    }
  }
}


object Nearest {
  def apply[T : SignedAdditiveAbGroup](points: Map[T, T]): Interpolant[T] = {
    val ordering = Order.catsKernelOrderingForOrder(implicitly[Order[T]])
    val tm = new TreeMap[T, T](ordering)
    points.foreach { case (k, v) =>
      tm.put(k, v)
    }
    new NearestImpl[T](tm)
  }
}
