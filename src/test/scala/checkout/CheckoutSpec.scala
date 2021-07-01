package checkout

import model.{Discount, ItemPricingRule}
import org.scalatest.FunSuite

class CheckoutSpec extends FunSuite {

  val rules = Map(
    "Apple" -> ItemPricingRule(unitPrice = BigDecimal("0.6"), discount = Some(Discount(minimumQuantity = 2, specialPrice = BigDecimal("0.6")))),
    "Orange" -> ItemPricingRule(unitPrice = BigDecimal("0.25"), discount = Some(Discount(minimumQuantity = 3, specialPrice = BigDecimal("0.5")))
  ))


  test("Check the price of two Apples") {
    val basket = Seq("Apple", "Apple")
    val checkout = Checkout(rules)

    basket.foreach(checkout.scanToAdd)
    assert(checkout.total equals BigDecimal("1.2"))
  }

  test("Check the price of Three Apples return the correct value with the applied discount") {
    val basket = Seq("Apple", "Apple", "Apple")
    val checkout = Checkout(rules)

    basket.foreach(checkout.scanToAdd)
    assert(checkout.total equals  BigDecimal("1.2"))
  }

  test("Calculate the price of four Apples with the applied discount") {
    val basket = Seq( "Apple", "Apple", "Apple", "Apple")
    val checkout = Checkout(rules)

    basket.foreach(checkout.scanToAdd)
    assert(checkout.total equals  BigDecimal("1.2"))
  }

  test("Calculate the price of three Oranges with the applied discount") {
    val basket = Seq("Orange", "Orange", "Orange")
    val checkout = Checkout(rules)

    basket.foreach(checkout.scanToAdd)
    assert(checkout.total equals  BigDecimal("0.5"))
  }

  test("Calculate the price of three Oranges and an Apple with the applied discount") {
    val basket = Seq("Orange", "Orange", "Orange", "Apple")
    val checkout = Checkout(rules)

    basket.foreach(checkout.scanToAdd)
    assert(checkout.total equals  BigDecimal("1.1"))
  }
}
