package uk.co.devproltd.checkout

import java.text.NumberFormat
import java.util.Locale

sealed trait Fruit {
  def price: Long
}
object Fruit {
  def parse(s: String): Option[Fruit] = s.trim.toLowerCase() match {
    case "apple"  => Some(Apple)
    case "orange" => Some(Orange)
    case _        => None
  }
}

case object Apple extends Fruit {
  override def price: Long = 60
}
case object Orange extends Fruit {
  override def price: Long = 25
}

object CheckoutSystem {

  def scanItems(itemNames: String*): List[Fruit] =
    itemNames.flatMap(Fruit.parse).toList

  def calculateTotal(basketContents: List[Fruit]): String = {
    def calculateOfferPrice(
        fruitCount: Int,
        buy: Int,
        payFor: Int,
        pricePerUnit: Long
    ): Long =
      (fruitCount / buy) * payFor * pricePerUnit + fruitCount % buy * pricePerUnit

    //count apples/oranges in one go
    val (appleCount, orangeCount) = basketContents.foldLeft((0, 0)) {
      case ((apples, oranges), Apple)  => (apples + 1, oranges)
      case ((apples, oranges), Orange) => (apples, oranges + 1)
    }

    val priceForApples =
      calculateOfferPrice(fruitCount = appleCount, buy = 2, payFor = 1, pricePerUnit = Apple.price)
    val priceForOranges =
      calculateOfferPrice(fruitCount = orangeCount, buy = 3, payFor = 2, pricePerUnit = Orange.price)

    val total = (priceForApples + priceForOranges).toDouble / 100
    NumberFormat.getCurrencyInstance(Locale.UK).format(total)
  }

}
