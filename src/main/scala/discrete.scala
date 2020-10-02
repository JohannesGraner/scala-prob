package scalaprob.probability

import scala.math.{abs, pow}

trait DiscreteTrait {

    def density: Map[Int, Double]

    def parameter: Any = None

    // Have to settle for 'close enough' due to floating point calculations.
    def checkDensity: Boolean = abs(density.values.sum - 1.0) < 1e-10
    def distribution: Map[Int, Double] = {
        density.keys.toSeq.sorted.scanLeft((0,0.0)){ case ((m,fm),n) => 
            (n, fm + density(n))
        }.tail.toMap
    }

    def moment(k: Int): Double = density.map( p => pow(p._1,k) * p._2 ).sum
    def mean: Double = moment(1)
    def variance: Double = moment(2) - pow(mean,2)

    def getProb(k: Int): Double = density.getOrElse(k, 0.0)

    def percentile(p: Double): Int = {
        if (p < 0 || p > 1)
            throw new IllegalArgumentException(s"$p is not between 0 and 1")
        else
            distribution.filter( _._2 <= p).map( _._1 ).max
    }

    protected def conv(n: Int, f: DiscreteTrait, g: DiscreteTrait): Double = {
        val (longDens, shortDens) =
            if (f.density.size > g.density.size)
                (f.density, g.density)
            else 
                (g.density, f.density)
        shortDens
            .map( p => longDens.getOrElse(n-p._1, 0.0)*p._2 )
            .sum
    }

    def convolution(other: DiscreteTrait): DiscreteTrait = {
        this.toDiscreteProb.convolution(other)
    }
        /*= {
        val thisKeys = this.density.keys
        val otherKeys = other.density.keys
        DiscreteProb(
            ( thisKeys.min + otherKeys.min to thisKeys.max + otherKeys.max )
                .map( n => (n, conv(n, this, other)) ).toMap 
        )
    }*/

    def print: Unit = println(density.toSeq.sortBy( _._1 ))

    def toDiscreteProb: DiscreteProb = DiscreteProb(density)
}

case class DiscreteProb(override val density: Map[Int, Double]) extends DiscreteTrait {

    override def convolution(other: DiscreteTrait): DiscreteProb = {
        val thisKeys = this.density.keys
        val otherKeys = other.density.keys
        DiscreteProb(
            ( thisKeys.min + otherKeys.min to thisKeys.max + otherKeys.max )
                .map( n => (n, conv(n, this, other)) ).toMap 
        )
    }
}

case class DeMoivre(k: Int) extends DiscreteTrait {
    override val parameter = k
    override val density: Map[Int, Double] = ((1 to k).toSeq zip Seq.fill(k)(1.0/k)).toMap
}

case class Bernoulli(p: Double) extends DiscreteTrait {
    override val parameter = p
    override val density = Map((0, 1-p), (1, p))

    override def convolution(other: DiscreteTrait): DiscreteTrait = {
        if (other.isInstanceOf[Bernoulli] && other.parameter == parameter)
            Binomial(p, 2)
        else
            this.toDiscreteProb.convolution(other)
    }
}

case class Binomial(p: Double, n: Int) extends DiscreteTrait {
    override val parameter: Any = (p, n)
    override val density = {
        val binoms = (2 to n).scanLeft(1)( (k, cnk) => (n + 1 - k)/k * cnk ).toSeq
        ((1 to n).toSeq zip binoms).map{ case (k: Int, cnk: Int) => (k,cnk*pow(p, k)*pow(1-p, n-k)) }.toMap
    }

    override def convolution(other: DiscreteTrait): DiscreteTrait = {
        if (other.isInstanceOf[Bernoulli] && other.parameter == p)
            Binomial(p, n+1)
        else if (other.isInstanceOf[Binomial] && other.parameter == parameter)
            Binomial (p, other.parameter.asInstanceOf[(Double, Int)]._2 + n)
        else 
            this.toDiscreteProb.convolution(other)
    }
}