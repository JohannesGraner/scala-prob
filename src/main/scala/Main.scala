package scalaprob.probability

object Main extends App {
  val testProb = DeMoivre(6)
  println(testProb.checkDensity)
  println(testProb.density)
  println(testProb.distribution)
  println(testProb.mean)
  println(testProb.variance)

  println()
  testProb.convolution(testProb).print
}