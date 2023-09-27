package tst

object ProblemOne extends App {

  case class Rate(rateCode: String, rateGroup: String)
  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
  case class BestGroupPrice(
      cabinCode: String,
      rateCode: String,
      price: BigDecimal,
      rateGroup: String
  )

  object BestGroupPrice {
    def apply(cpr: CabinPriceWithRate): BestGroupPrice = {
      BestGroupPrice(
        cpr.cabinPrice.cabinCode,
        cpr.cabinPrice.rateCode,
        cpr.cabinPrice.price,
        cpr.rate.rateGroup
      )
    }
  }

  private final case class CabinPriceWithRate(
      cabinPrice: CabinPrice,
      rate: Rate
  )
  def getBestGroupPrices(
      rates: Seq[Rate],
      prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] = {
    prices
      .flatMap { price =>
        rates
          .find(_.rateCode == price.rateCode)
          .map(rate => CabinPriceWithRate(price, rate))
      }
      .groupBy(_.cabinPrice.cabinCode)
      .values
      .flatMap { cprs =>
        cprs.groupBy(_.rate.rateGroup).values.map { as =>
          val min = as.minBy(_.cabinPrice.price)
          BestGroupPrice(min)
        }
      }
      .toSeq
      .sortBy(bgp => (bgp.cabinCode, bgp.price))
  }

  val rates = Seq(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val prices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )
  val results = getBestGroupPrices(rates, prices)
  results.foreach(println)
}
