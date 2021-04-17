import scala.math.sqrt
object Kleisli {
  // 1. Construct the Kleisli category for partial functions (define composition
  //    and identity).
  object Partial {
    def compose[A, B, C](
        fa: A => Option[B],
        fb: B => Option[C]
    ): A => Option[C] = a =>
      fa(a) match {
        case None    => None
        case Some(b) => fb(b)
      }

    def id[A](a: A): A = a
  }

  // 2. Implement the embellished function safe_reciprocal that returns a valid
  //    reciprocal of its
  //    argument, if it’s different from zero.
  def safeRoot(x: Double): Option[Double] = if (x < 0) None else Some(sqrt(x))
  def safeReciprocal(x: Double): Option[Double] = x match {
    case 0.0 => None
    case _   => Some(1 / x)
  }

  // 3. Compose the functions safe_root and safe_reciprocal to
  //    implement safe_root_reciprocal that calculates sqrt(1/x)
  //    whenever possible.
  val safeReciprocaleRoot = Partial.compose(safeRoot, safeReciprocal)
}
