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
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  implicit class FunctorOps[A,F[_]:Functor](fa: F[A]) {
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].map(fa)(f)
  }
}

object FunctorExtra {
  implicit class InfixFunctorOps[A,B](f: A => B) {
    def <%>[F[_]:Functor](fa: F[A]): F[B] =
      implicitly[Functor[F]].map(fa)(f)
  }
}

trait Applicative[F[_]] extends Functor[F] {
  def ap[A,B](ff: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  implicit class ApplicativeOps[A,F[_]:Applicative](fa: F[A]) {
    def ap[B](ff: F[A => B]): F[B] =
      implicitly[Applicative[F]].ap(ff)(fa)
  }
}

object ApplicativeExtra {
  implicit class InfixApplicativeOps[A,B,F[_]:Applicative](ff: F[A => B]) {
    def <*>(fa: F[A]): F[B] = implicitly[Applicative[F]].ap(ff)(fa)
  }
}


sealed trait Validation[E,A]
case class Success[E,A](value: A) extends Validation[E,A]
case class Failure[E,A](value: E) extends Validation[E,A]

object Validation {

  implicit def applicative[E:Semigroup]: Applicative[({type λ[α] = Validation[E,α]})#λ] =
    new Applicative[({type λ[α] = Validation[E,α]})#λ] {
      def map[A,B](fa: Validation[E,A])(f: A => B): Validation[E,B] =
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

}

case class User private (name: String, email: String, phone: String)

object User {

  type Parsed[A] = Validation[List[String], A]

  private def parseR(n: String, x: String, r: Regex): Parsed[String] =
    r.findFirstIn(x) match {
      case Some(_) => Success(x)
      case _       => Failure(List(s"""invalid ${n}: "${x}""""))
    }

  import Functor.FunctorOps
  import Applicative.ApplicativeOps

  def parse(name: String, email: String, phone: String): Parsed[User] =
    parseR("name", name, """\w+(\s\w+)*""".r).ap(
      parseR("email", email, """[^@]+@[^@]+""".r).ap(
        parseR("phone", phone, """\d{3}-\d{3}-\d{4}""".r).map(
          (User.apply _).curried
        )
      )
    )

  import FunctorExtra.InfixFunctorOps
  import ApplicativeExtra.InfixApplicativeOps

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
