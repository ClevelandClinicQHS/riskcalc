test_that("I get a shiny app", {
  expect_equal(class(risk_calculator(glm(1~1))), "shiny.appobj")
})

test_that("I get an error for specifying multiple time points", {
  mod <- survival::coxph(survival::Surv(time, status) ~ x, data = survival::aml)
  expect_error(risk_calculator(mod, times = c(10, 20)))
})
