package net.degoes.afd.examples.commerce

final case class Product(id: String, mfg: Manufacturer, model: String, name: String, price: Double, dept: Department)

final case class Manufacturer(id: String, name: String)
object Manufacturer {
  val Sony    = Manufacturer("sony", "Sony")
  val Samsung = Manufacturer("samsung", "Samsung")
}

sealed trait Department
object Department {
  case object Books      extends Department
  case object Music      extends Department
  case object Movies     extends Department
  case object Appliances extends Department
}

object recommendations {
  trait RecommendationStrategy {
    def recommend(product: Product): List[Product]
  }
  case object CrossSell extends RecommendationStrategy {
    override def recommend(product: Product): List[Product] =
      product match {
        case Product(_, Manufacturer.Samsung, model, name, _, Department.Appliances) =>
          if (model == "Series 9" && name.toLowerCase.contains("washing machine")) {
            List(
              Product(
                "samsung-series-9-dryer",
                Manufacturer.Samsung,
                "Series 9",
                "Samsung Series 9 Dryer",
                999.99,
                Department.Appliances
              )
            )
          } else Nil
        case _ => Nil
      }
  }
}
