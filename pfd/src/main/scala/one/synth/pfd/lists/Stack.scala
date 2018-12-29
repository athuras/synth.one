package one.synth.pfd.lists

import annotation.tailrec

/**
 * The base interface for Stack behavior.
 *
 * The intent here is for implementations to be implemented statically in terms of these methods
 * (see {{ ListStack }} for an example). This was more of an experiment to see if this style is useful.
 *
 * For instance-methods (i.e. concrete), check out `Stack.Instance`.
 **/
trait Stack[S[_]] {
  import Stack._

  def empty[T](): S[T]
  def isEmpty(s: S[_]): Boolean
  def cons[T](a: T, b: S[T]): S[T]
  def head[T](s: S[T]): T
  def tail[T](s: S[T]): S[T]

  // Extensions

  def concat[T](a: S[T], b: S[T]): S[T] = {
    if (isEmpty(a)) b
    else cons(head(a), concat(tail(a), b))
  }

  def update[T](s: S[T], i: Int, y: T): S[T] =
    if (isEmpty(s) | i < 0) {
      throw OutOfBounds()
    }
    else if (i == 0) {
      cons(y, tail(s))
    }
    else {
      cons(head(s), update(tail(s), i - 1, y))
    }

  def suffixes[T](xs: List[T]): List[List[T]] = {
    @tailrec
    def popAll(ys: List[T], acc: S[List[T]]): S[List[T]] = ys match {
      case Nil => cons(Nil, acc)
      case _ :: xs => popAll(xs, cons(ys, acc))
    }
    val popped = popAll(xs, empty())
    // [1, 2], [Nil]
    // [2], [[1,2]]
    // [], [2, [1, 2]]
    toList(popped, Nil)
  }

  protected def fromList[T](xs: List[T], acc: S[T]): S[T] = xs match {
    case Nil => acc
    case y :: ys =>
      fromList(ys, cons(y, acc))
  }

  protected def toList[T](s: S[T], acc: List[T]): List[T] = {
    if (isEmpty(s)) acc
    else toList(tail(s), head(s) :: acc)
  }
}

object Stack {
  protected final case class OutOfBounds() extends RuntimeException
  protected final case class EmptyStack() extends RuntimeException

  implicit case object ListStack extends Stack[List] {

    override def empty[T]() = Nil
    override def isEmpty(s: List[_]) = s.isEmpty
    override def cons[T](a: T, b: List[T]) = a :: b
    override def head[T](a: List[T]) = a.head
    override def tail[T](a: List[T]) = a.tail

    override def concat[T](a: List[T], b: List[T]) = a ++ b
  }

  def of[T, Repr[_] : Stack](r: Repr[T]): Instance[T, Repr] =
    new Instance(implicitly[Stack[Repr]], r)

  final case class Instance[T, Repr[_]](
    stack: Stack[Repr],
    data: Repr[T],
  ) {
    def isEmpty: Boolean = stack.isEmpty(data)

    def cons(a: T): Instance[T, Repr] =
      new Instance(stack, stack.cons(a, data))

    def head: T = stack.head(data)

    def tail: Instance[T, Repr] =
      new Instance(stack, stack.tail(data))

    // Extensions
    def concat(s: Repr[T]): Instance[T, Repr] =
      new Instance(stack, stack.concat(data, s))

    def update(i: Int, y: T): Instance[T, Repr] =
      new Instance(stack, stack.update(data, i, y))

    lazy val toList: List[T] = stack.toList(data, Nil)
  }
}
