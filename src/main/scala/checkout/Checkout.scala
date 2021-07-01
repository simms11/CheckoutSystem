package checkout

case class Checkout(pricingRules: Map[String, ItemPricingRule]) {
  var basket: Map[String, Int] = Map()

  def scanToAdd(item: String): Map[String, Int] = {
    basket += (item -> (basket.getOrElse(item, 0) + 1))
    basket
  }


  def total(): BigDecimal = {
    basket.toList.map { item =>
      val itemPricingRule = pricingRules.get(item._1)

      itemPricingRule.get.unitPrice * itemPricingRule.get.quantity
    }.sum
  }


}