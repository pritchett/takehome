package tst

class ProblemTwoSpec extends munit.FunSuite {
  import ProblemTwo._
  test("expected results from problem sheet for allCombinablePromotions") {

    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    val actual = allCombinablePromotions(promotions)
    assertEquals(actual, expected)
  }

  test("expected results from problem sheet for combinablePromotions") {

    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    val actual = combinablePromotions("P1", promotions)
    assertEquals(actual, expected)
  }

  test("expected results from problem sheet for combinablePromotions (2)") {

    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = Seq(
      // This first combo is different than what was on the problem sheet,
      // but I'm assuming its still supposed to be in this order
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    val actual = combinablePromotions("P3", promotions)
    assertEquals(actual, expected)
  }

  test("results with an invalid promo code is empty") {

    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = Seq.empty[PromotionCombo]

    val actual = combinablePromotions("test", promotions)
    assertEquals(actual, expected)
  }
}
