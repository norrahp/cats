import cats.Monoid
import cats.implicits.catsSyntaxSemigroup
import cats.instances.int._

// Ex 2.5.4
def add1(items: List[Int]): Int =
  items.foldLeft(0)(_ + _)

def add[A](items: List[A])(implicit ev: Monoid[A]): A =
  items.foldLeft(ev.empty)(_ |+| _)

case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid = new Monoid[Order] {
  override def empty = Order(0, 0)
  override def combine(o1: Order, o2: Order) =
    Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)
}

println(add(List(1, 2, 3)))
println(add(List(Order(2, 2), Order(3, 1))))

import cats.instances.option._
println(add(List[Option[Int]](Some(2), Some(1))))
println(add(List[Option[Int]](None, Some(1))))
