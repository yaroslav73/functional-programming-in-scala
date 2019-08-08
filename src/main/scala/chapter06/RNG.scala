package chapter06

trait RNG {
  def nextInt: (Int, RNG)
}
