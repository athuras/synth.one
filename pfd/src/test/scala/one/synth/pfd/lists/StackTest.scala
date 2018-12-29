package one.synth.pfd.lists

import org.scalatest._

class StackTest extends FlatSpec with DiagrammedAssertions {

  behavior of "Stack"

  it should "Produce Suffixes Correctly" in {
    val example = List(1, 2, 3, 4)
    val expected = List(
      List(1, 2, 3, 4),
      List(2, 3, 4),
      List(3, 4),
      List(4),
      Nil
    )
    assert(Stack.ListStack.suffixes(example) === expected)
  }
}
