package probability

object Main extends App {
  val testProb = DiscreteProb(((1 to 6).toSeq zip Seq.fill(6)(1.0/6)).toMap)
  println(testProb.checkDensity)
  println(testProb.density)
  println(testProb.distribution)
  println(testProb.mean)
  println(testProb.variance)

  println()
  testProb.convolution(testProb).print
}