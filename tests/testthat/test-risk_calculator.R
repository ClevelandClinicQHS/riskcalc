test_that("I get a shiny app", {
  expect_equal(class(risk_calculator(NULL, NULL)), "shiny.appobj")
})
