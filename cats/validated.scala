import cats.implicits.listAlgebra // Monoid[List[_]] 
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.Functor
import cats.Applicative
import scala.util.matching.Regex

object FunctorExtra {
  implicit class FlippedFunctorOps[A,B](f: A => B) {
    def <%>[F[_]:Functor](fa: F[A]): F[B] =
      implicitly[Functor[F]].map(fa)(f)
  }
}

object ApplicativeExtra {
  implicit class FlippedApplicativeOps[A,B,F[_]:Applicative](ff: F[A => B]) {
    def <*>(fa: F[A]): F[B] = implicitly[Applicative[F]].ap(ff)(fa)
  }
}

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

  import FunctorExtra.FlippedFunctorOps
  import ApplicativeExtra.FlippedApplicativeOps

  def parse2(name: String, email: String, phone: String): Parsed[User] =
    (User.apply _).curried <%>
      parseR("name", name, """\w+(\s\w+)*""".r) <*>
      parseR("email", email, """[^@]+@[^@]+""".r) <*>
      parseR("phone", phone, """\d{3}-\d{3}-\d{4}""".r)

}

object Main extends App {

  def demo(parse: (String, String, String) => User.Parsed[User],
           name: String, email: String, phone: String): Unit = {
    println(s"""parse("${name}", "${email}", "${phone}"):""")
    parse(name, email, phone) match {
      case Valid(x)    => println(s"  ${x}\n")
      case Invalid(es) => println(es.mkString("  ", "\n  ", "\n"))
    }
  }

  println()

  demo(User.parse _, "", "", "")
  demo(User.parse _, "James", "james", "555-JAMES-42")
  demo(User.parse _, "James", "james@earldouglas.com", "555-JAMES-42")
  demo(User.parse _, "James", "james@earldouglas.com", "555-123-4567")

  demo(User.parse2 _, "", "", "")
  demo(User.parse2 _, "James", "james", "555-JAMES-42")
  demo(User.parse2 _, "James", "james@earldouglas.com", "555-JAMES-42")
  demo(User.parse2 _, "James", "james@earldouglas.com", "555-123-4567")

}
