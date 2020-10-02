package probability

import scala.math.abs

class DiscreteTest extends org.scalatest.funsuite.AnyFunSuite {

    val d6 = DiscreteProb(((1 to 6).toSeq zip Seq.fill(6)(1.0/6)).toMap)

    def approxComp(a: Double, b: Double, tol: Double = 1e-10): Boolean = {
        abs(a - b) < tol
    }

    test("1D6") {
        assert(d6.checkDensity)
        assert(approxComp(d6.mean, 3.5))
        assert(approxComp(d6.variance, 91.0/6-49.0/4))
        assert(d6.getProb(7) == 0)
    }

    test("2d6") {
        val twoD6 = d6.convolution(d6)
        assert(twoD6.checkDensity)
        assert(approxComp(twoD6.mean, 7))
        assert(approxComp(twoD6.getProb(7), 1.0/6))
    }

    test("10d6") {
        val tenD6: DiscreteProb = Seq.fill(9)(d6).fold(d6)(_.convolution(_))
        assert(tenD6.checkDensity)
        assert(approxComp(tenD6.mean, 5*7))
        assert(tenD6.getProb(9) == 0)
    }

}