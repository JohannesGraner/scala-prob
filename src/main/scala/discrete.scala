package probability

case class DiscreteProb(
    density: Map[Int, Double]
) {
    // Have to settle for 'close enough' due to floating point calculations.
    def checkDensity: Boolean = Math.abs(density.values.sum - 1.0) < 1e-10
    def distribution: Map[Int, Double] = {
        density.keys.toSeq.sorted.scanLeft((0,0.0)){ case ((m,fm),n) => 
            (n, fm + density(n))
        }.tail.toMap
    }

    def percentile(p: Double): Int = {
        if (p < 0 || p > 1)
            throw new IllegalArgumentException(s"$p is not between 0 and 1")
        else
            distribution.filter( _._2 <= p).map( _._1 ).max
    }

    private def conv(n: Int, f: DiscreteProb, g: DiscreteProb): Double = {
        val (longDens, shortDens) =
            if (f.density.size > g.density.size)
                (f.density, g.density)
            else 
                (g.density, f.density)
        shortDens
            .map{ case (m: Int, gm: Double) => longDens.get(n-m).getOrElse(0.0)*gm }
            .sum
    }

    def convolution(other: DiscreteProb): DiscreteProb = {
        val thisKeys = this.density.keys
        val otherKeys = other.density.keys
        DiscreteProb(
            ( thisKeys.min + otherKeys.min to thisKeys.max + otherKeys.max )
                .map( n => (n, conv(n, this, other)) ).toMap )
    }

    def print: Unit = println(density.toSeq.sortBy( _._1 ))
}
