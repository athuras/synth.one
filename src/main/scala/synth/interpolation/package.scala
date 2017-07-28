package synth

package object interpolation {
  type Interpolant[T] = PartialFunction[T, T]
}
