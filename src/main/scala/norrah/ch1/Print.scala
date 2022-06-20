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

  val c = Cat("Peggy", 1, "white")

  object PrintableSyntax {

    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String =
        p.format(value)

      def print(implicit p: Printable[A]): Unit =
        println(format)
    }
  }

  Printable.print(c)

  import PrintableSyntax._
  // or with a nice syntax
  c.print

}
