package uk.co.devproltd.checkout

class CheckoutSystemSpec extends BaseUnitSpec {

  class Basket(val s: String*) {
    lazy val basketContents: List[Item] = CheckoutSystem.scanItems(s: _*)
  }

  "a list of string inputs" should {
    "convert to a list of apples and oranges" in {
      val items = CheckoutSystem.scanItems(
        "apple",
        "orange",
        "APPLE",
        "ORANGE",
        "carrot",
        " applE",
        "oRange "
      )
      //expect 3 of each, capitalisation or leading/trailing whitespace should not matter
      items.count(_.name == "apple") shouldBe 3
      items.count(_.name == "orange") shouldBe 3

      //should be no carrots in there, as our shop doesn't sell those
      items.exists(_.name == "carrot") shouldBe false

    }
  }

  "a shopping list of apples and oranges" should {
    "total up to the correct price" when {

      "the basket is empty" in new Basket() {
        CheckoutSystem.calculateTotal(basketContents) shouldBe 0L
      }

      "the basket contains some apples and oranges" in new Basket(
        "apple",
        "apple",
        "orange",
        "apple"
      ) {
        CheckoutSystem.calculateTotal(basketContents) shouldBe 205L
      }

      "the basket contains apples and carrots" in new Basket("apple", "APPLE", "carrot") {
        CheckoutSystem.calculateTotal(basketContents) shouldBe 120L //carrots are ignored
      }
    }

  }
}
