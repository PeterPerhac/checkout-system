package uk.co.devproltd.checkout

import uk.co.devproltd.checkout.CheckoutSystem.calculateTotal

class CheckoutSystemSpec extends BaseUnitSpec {

  class Basket(val s: String*) {
    lazy val basketContents: List[Fruit] = CheckoutSystem.scanItems(s: _*)
  }

  "a list of string inputs" should {
    "convert to a list of apples and oranges" in {
      val toCheckout = List("apple", "orange", "APPLE", "ORANGE", "carrot", " applE", "oRange ")
      val items = CheckoutSystem.scanItems(toCheckout: _*)

      items should have size 6
      //expect 3 of each
      items.count(_ == Apple) shouldBe 3
      items.count(_ == Orange) shouldBe 3

    }
  }

  "a shopping basket of apples and oranges" should {

    "total up to the correct price" when {

      "it is empty" in new Basket() {
        calculateTotal(basketContents) shouldBe "£0.00"
      }

      "it contains apples and carrots" in new Basket(
        "apple",
        "carrot"
      ) {
        //carrots are ignored
        calculateTotal(basketContents) shouldBe "£0.60"
      }

      "it contains 6 oranges" in new Basket("orange", "orange", "orange", "orange", "orange", "orange") {
        calculateTotal(basketContents) shouldBe "£1.00"
      }

      "it contains a selection of fruit" in new Basket(
        "apple",
        "orange",
        "apple", //second apple free
        "orange",
        "apple",
        "orange", //third orange free
        "orange"
      ) {
        calculateTotal(basketContents) shouldBe "£1.95" // price of two apples and three oranges
      }

    }

    "cost the same" when {
      "adding the right amount of fruit to one's basket to make use of the available offers" in {
        val basicPurchase = CheckoutSystem.scanItems("apple", "orange", "orange")
        val cleverPurchase = CheckoutSystem.scanItems("apple", "apple", "orange", "orange", "orange")

        calculateTotal(basicPurchase) shouldEqual calculateTotal(cleverPurchase)
      }
    }
  }

}
