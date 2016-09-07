package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(),2) - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(solutions(a(),b(),c(),delta()))
  }

  private def solutions(a: Double, b: Double, c: Double, delta: Double): Set[Double] = {
    var ans = Set[Double]()
    if (delta > 0.0) {
      val sqrtDelta: Double = Math.sqrt(delta)
      ans += ((-b+sqrtDelta/(2*a))); ans += ((-b-sqrtDelta/(2*a)))
    }
    else if (delta == 0.0) ans += (-b/(2*a))
//    delta match {
//      case a if a > 0.0  => ans += ((-b+Math.sqrt(delta)/4*a)); ans += ((-b-Math.sqrt(delta)/4*a))
//      case a if a < 0.1 && a >=0.0 => ans += (-b/4*a)
//    }
    ans
  }


}
