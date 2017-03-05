package com.foldright

import cats.implicits._
import cats.{Monoid, Show}

sealed trait Fruit {
  def price: BigDecimal = this match {
    case Apple => 60
    case Orange => 25
  }
}

case object Apple extends Fruit

case object Orange extends Fruit

case class Checked(map: Map[Fruit, Int]) {

  private def calculatePrice(f: Fruit, buy: Int, free: Int): BigDecimal = {
    val size = map.getOrElse(f, 0)
    ((size % buy) + ((size / buy) * free)) * f.price
  }

  def totalSum: BigDecimal = List((Apple, 2, 1), (Orange, 3, 2)).map((calculatePrice _).tupled).sum
}

object Checked {

  implicit val bigDecimalShow = Show.show((d: BigDecimal) => "£" + (d / 100.0))

  implicit val m = new Monoid[Checked] {
    def combine(c1: Checked, c2: Checked): Checked = Checked(c1.map |+| c2.map)

    def empty: Checked = Checked(Map.empty[Fruit, Int])
  }

}

object CheckoutSystem {
  def toChecked(f: Fruit): Checked = Checked(Map(f -> 1))

  def checkout(fruits: List[Fruit]): BigDecimal = Monoid[Checked].combineAll(fruits.map(toChecked)).totalSum
}
