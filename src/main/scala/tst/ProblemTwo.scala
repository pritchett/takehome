package tst

object ProblemTwo extends App {
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  val promotions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with P2
  )

  type PromotionGraph = Map[Promotion, Seq[Promotion]]

  private def buildGraph(promotions: Seq[Promotion]): PromotionGraph =
    promotions
      .map(promo =>
        promo -> promotions.filterNot(p =>
          p.notCombinableWith.contains(promo.code)
        )
      )
      .toMap

  private def combinablePromotionsFromGraph(
      graph: PromotionGraph
  )(promo: Promotion): Seq[PromotionCombo] =
    graph(promo)
      .flatMap { p =>
        val nodes =
          graph(p).filterNot(_.notCombinableWith.contains(promo.code))
        val notCombinable = nodes.flatMap(_.notCombinableWith)
        val nodeCodes = nodes.map(_.code)

        // If anything in the collection of nodes cannot be combined
        // then it should be excluded
        Option.when(notCombinable.intersect(nodeCodes).isEmpty)(
          nodeCodes.toSet + p.code
        )

      }
      .map(codes => PromotionCombo(codes.toSeq.sorted))
      .sortBy(_.promotionCodes.mkString)

  def allCombinablePromotions(
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {

    val graph = buildGraph(allPromotions)
    val getCombineablePromotions = combinablePromotionsFromGraph(graph)(_)
    allPromotions
      .flatMap { promo =>
        getCombineablePromotions(promo)
      }
      .toSet
      .toSeq
  }

  def combinablePromotions(
      promotionCode: String,
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val graph = buildGraph(allPromotions)
    val promo = allPromotions.find(_.code == promotionCode)
    promo
      .map(p => combinablePromotionsFromGraph(graph)(p).toSet.toSeq)
      .getOrElse(Seq.empty)
  }

  println("Output for All Promotion Combinations")
  allCombinablePromotions(promotions).foreach(println)

  println("Output for Promotion Combinations for promotionCode=”P1”")
  combinablePromotions("P1", promotions).foreach(println)

  println("Output for Promotion Combinations for promotionCode=”P3”")
  combinablePromotions("P3", promotions).foreach(println)
}
