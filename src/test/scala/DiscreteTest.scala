package scalaprob.probability

import scala.math.abs
import spire.math.Rational

class DiscreteTest extends org.scalatest.funsuite.AnyFunSuite {

  val d6 = DeMoivre(6)
  val ber = Bernoulli(Rational(1,3))
  val bin = Binomial(Rational(1,3), 3)

  test("Shifting") {
    assert(d6.shift(2).getProb(7) == Rational(1,6))
    println(d6.shift(2))
  }

  test("Probability of Set") {
    assert(d6.getProb(Set(1,2)) == Rational(1,3))
  }

  test("1D6") {
    assert(d6.checkDensity)
    assert(d6.mean == 3.5)
    assert(d6.variance == Rational(91, 6) - Rational(49, 4))
    assert(d6.getProb(7) == 0)
  }

  test("2d6") {
    val twoD6 = d6.convolution(d6)
    assert(twoD6.checkDensity)
    assert(twoD6.mean == 7)
    assert(twoD6.getProb(7) == Rational(1, 6))
  }

  test("10d6") {
    val tenD6 = Seq.fill(9)(d6).foldLeft(d6.toDiscreteProb) {
      case (p1: DiscreteProb, p2: DiscreteTrait) => p1.convolution(p2)
    }
    assert(tenD6.checkDensity)
    assert(tenD6.mean == 5 * 7)
    assert(tenD6.getProb(9) == 0)
  }

  test("bernoulli") {
    assert(ber.convolution(ber) == Binomial(Rational(1,3), 2))
  }

  test("Binomial") {
    assert(bin.checkDensity)
    assert(bin.getProb(2) == 3*Rational(2,27))
    assert(bin.convolution(bin) == Binomial(Rational(1,3), 6))
  }

}
