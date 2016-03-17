import scala.util.matching.Regex

trait Semigroup[A] {
  def append(x: A)(y: A): A
}

object Semigroup {
  implicit def listSemigroup[A]: Semigroup[List[A]] =
    new Semigroup[List[A]] {
      def append(x: List[A])(y: List[A]): List[A] = x ++ y
    }
}

trait Functor[F[_]] {
  def map[A,B](f: A => B)(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def ap[A,B](ff: F[A => B])(fa: F[A]): F[B]
}

sealed trait Validation[E,A]
case class Success[E,A](value: A) extends Validation[E,A]
case class Failure[E,A](value: E) extends Validation[E,A]

object Validation {

  implicit def applicative[E:Semigroup]: Applicative[({type λ[α] = Validation[E,α]})#λ] =
    new Applicative[({type λ[α] = Validation[E,α]})#λ] {
      def map[A,B](f: A => B)(fa: Validation[E,A]): Validation[E,B] =
        fa match {
          case Success(a)  => Success(f(a))
          case Failure(es) => Failure(es)
        }
      def ap[A,B](ff: Validation[E,A => B])(fa: Validation[E,A]): Validation[E,B] =
        fa match {
          case Success(a) =>
            ff match {
              case Success(f)  => Success(f(a))
              case Failure(es) => Failure(es)
            }
          case Failure(es) =>
            ff match {
              case Success(f)  => Failure(es)
              case Failure(es2) => Failure(implicitly[Semigroup[E]].append(es2)(es))
            }
        }
    }

  implicit class ValidationOps[E:Semigroup,A](fa: Validation[E,A]) {
    def map[B](f: A => B): Validation[E,B] =
      applicative[E].map(f)(fa)
    def ap[B](ff: Validation[E,A => B]): Validation[E,B] =
      applicative[E].ap(ff)(fa)
  }

  implicit class FnCofunctor[A,B](f: A => B) {
    def <%>[E:Semigroup](fa: Validation[E,A]) =
      applicative[E].map(f)(fa)
  }

  implicit class ValidationCofunctor[A,B,E:Semigroup](ff: Validation[E,A => B]) {
    def <*>(fa: Validation[E,A]) =
      applicative[E].ap(ff)(fa)
  }
 
}

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

  import Validation._

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
      case Success(x)  => println(s"  ${x}\n")
      case Failure(es) => println(es.mkString("  ", "\n  ", "\n"))
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
