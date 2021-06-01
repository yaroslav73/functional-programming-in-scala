package chapter01

final case class CreditCard(pan: String) {
  def charge(price: Double): Unit =
    println(s"Charged $price from your credit card")
}

final case class Coffee() {
  val price: Double = 1.99
}

final case class Payments() {
  def charge(cc: CreditCard, price: Double): Unit =
    println(s"Charged $price from your credit card: $cc")
}

final case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc) Charge(cc, amount + other.amount)
    else throw new Exception("Can't combine charges to different cards.")
}
