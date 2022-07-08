package norrah.ch1

object Print extends App {

  // type class
  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {

    implicit val intInstance = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

    implicit val stringInstance = new Printable[String] {
      override def format(value: String): String = value
    }
  }

  object Printable {

    def format[A](a: A)(implicit printable: Printable[A]): String =
      printable.format(a)

    def print[A](a: A)(implicit printable: Printable[A]): Unit =
      println(format(a))

  }

  final case class Cat(name: String, age: Int, color: String)

  import PrintableInstances._
  implicit val catInstance = new Printable[Cat] {
    override def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }

  object PrintableSyntax {

    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String =
        p.format(value)

      def print(implicit p: Printable[A]): Unit =
        println(format)
    }
  }

  val cat = Cat("Peggy", 1, "white")

  Printable.print(cat)
  import PrintableSyntax._
  // or with a nice syntax
  cat.print

  // Ex 1.4.6
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val showCat: Show[Cat] = (c: Cat) => {
    val name = c.name.show
    val age = c.age.show
    val color = c.color.show
    s"$name is a $age year-old $color cat."
  }

  println(cat.show)

  // Ex 1.5.5
  import cats.Eq
  import cats.implicits.catsSyntaxEq

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
    }

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(s"Are they equal? ${cat1 === cat2}")

  println(cat1 =!= cat2)

  println(optionCat1 === optionCat2)

  println(optionCat1 =!= optionCat2)

}
