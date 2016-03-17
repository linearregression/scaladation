import cats.implicits.listAlgebra // Monoid[List[_]] 
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import scala.util.matching.Regex

case class User private (name: String, email: String, phone: String)

object User {

  type Parsed[A] = Validated[List[String], A]

  private def parseR(n: String, x: String, r: Regex): Parsed[String] =
    r.findFirstIn(x) match {
      case Some(_) => Valid(x)
      case _       => Invalid(List(s"""invalid ${n}: "${x}""""))
    }

  def parse(name: String, email: String, phone: String): Parsed[User] =
    parseR("name", name, """\w+(\s\w+)*""".r).ap(
      parseR("email", email, """[^@]+@[^@]+""".r).ap(
        parseR("phone", phone, """\d{3}-\d{3}-\d{4}""".r).map(
          (User.apply _).curried
        )
      )
    )

}

object Main extends App {

  def demo(name: String, email: String, phone: String): Unit = {
    println(s"""User.parse("${name}", "${email}", "${phone}"):""")
    User.parse(name, email, phone) match {
      case Valid(x)    => println(s"  ${x}\n")
      case Invalid(es) => println(es.mkString("  ", "\n  ", "\n"))
    }
  }

  println()

  demo("", "", "")
  demo("James", "james", "555-JAMES-42")
  demo("James", "james@earldouglas.com", "555-JAMES-42")
  demo("James", "james@earldouglas.com", "555-123-4567")

}
