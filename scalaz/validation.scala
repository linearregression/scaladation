import scalaz.std.list.listMonoid // Monoid[List[_]]
import scalaz.Validation
import scalaz.Failure
import scalaz.Success
import scala.util.matching.Regex

case class User private (name: String, email: String, phone: String)

object User {

  type Parsed[A] = Validation[List[String], A]

  private def parseR(n: String, x: String, r: Regex): Parsed[String] =
    r.findFirstIn(x) match {
      case Some(_) => Success(x)
      case _       => Failure(List(s"""invalid ${n}: "${x}""""))
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
      case Success(x)    => println(s"  ${x}\n")
      case Failure(es) => println(es.mkString("  ", "\n  ", "\n"))
    }
  }

  println()

  demo("", "", "")
  demo("James", "james", "555-JAMES-42")
  demo("James", "james@earldouglas.com", "555-JAMES-42")
  demo("James", "james@earldouglas.com", "555-123-4567")

}
