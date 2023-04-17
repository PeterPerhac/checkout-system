package uk.co.devproltd.checkout

import java.text.NumberFormat
import java.util.Locale

case class Item(name: String, price: Long)

object CheckoutSystem {

  private val currencyFormatter = NumberFormat.getCurrencyInstance(Locale.UK)
  private val priceList: Map[String, Long] = Map(
    "apple" -> 60L,
    "orange" -> 25L
  )

  def scanItems(itemNames: String*): List[Item] = {
    def scanItem(itemName: String): Option[Item] = {
      val normalisedName = itemName.trim.toLowerCase()
      priceList.get(normalisedName).map(price => Item(normalisedName, price))
    }
    itemNames.flatMap(scanItem).toList
  }

  def calculateTotal(basketContents: List[Item]): Long = {
    basketContents.foldLeft(0L)((subtotal, item) => subtotal + item.price)
  }

  def formatPrice(priceInPence: Long): String =
    currencyFormatter.format(priceInPence.toDouble / 100)

}
