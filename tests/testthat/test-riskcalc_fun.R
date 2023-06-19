test_that("I get a shiny app", {
  expect_equal(class(riskcalc_fun()), "shiny.appobj")
})
