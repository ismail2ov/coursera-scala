package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bValue = b()
      (bValue * bValue) - 4 * (a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val negativeB = Signal(-1 * b())
    val doubleA = Signal(2 * a())
    val sqrtDelta = Signal(math.sqrt(delta()))

    Signal {
      if (delta() < 0) Set()
      else {
        Set(
          (negativeB() + sqrtDelta()) / doubleA(),
          (negativeB() - sqrtDelta()) / doubleA()
        )
      }
    }
  }
}
