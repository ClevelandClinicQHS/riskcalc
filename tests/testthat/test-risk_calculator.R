test_that("I get a shiny app", {
  expect_equal(class(risk_calculator(glm(1~1))), "shiny.appobj")
})
