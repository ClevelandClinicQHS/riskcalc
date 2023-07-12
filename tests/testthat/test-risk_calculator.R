test_that("I get a shiny app", {
  expect_equal(class(risk_calculator(list(), NULL)), "shiny.appobj")
})
