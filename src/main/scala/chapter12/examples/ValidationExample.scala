package chapter12.examples

import chapter12.Validation
import chapter12.Validation._

import java.util.Date

object ValidationExample extends App {
  final case class WebForm(name: String, birthDate: Date, phone: String)

  def validName(name: String): Validation[String, String] =
    if (name.trim != "") Success(name) else Failure("Name cannot be empty.")

  def validBirthdate(birthDate: String): Validation[String, Date] = {
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthDate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
    }
  }

  def validPhone(phone: String): Validation[String, String] =
    if (phone.matches("[0-9]{10}")) Success(phone) else Failure("Phone number must be 10 digits")

  def validatedWebForm(name: String, birthDate: String, phone: String): Validation[String, WebForm] =
    applicativeValidation.map3(
      validName(name),
      validBirthdate(birthDate),
      validPhone(phone)
    )(WebForm)

  val success = validatedWebForm("Tom", "2000-01-02", "1234567890")
  val failure = validatedWebForm("", "2000-March-02", "123456789123")

  println(success)
  println(failure)
}
