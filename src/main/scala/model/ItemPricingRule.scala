package model

case class ItemPricingRule(unitPrice: BigDecimal, discount: Option[Discount] = None)
