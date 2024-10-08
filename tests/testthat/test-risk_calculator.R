test_that("Nothing returns", {
  expect_equal(risk_calculator(), NULL)
})

test_that("I get an error for specifying multiple time points", {
  mod <- survival::coxph(survival::Surv(time, status) ~ x, data = survival::aml)
  expect_error(risk_calculator(mod, times = c(10, 20)))
})

test_that("I get an error for negating terms in the model formula", {
  mod <- survival::coxph(survival::Surv(rfstime, status) ~ . - age, data = survival::gbsg)
  expect_error(risk_calculator(mod, times = 10))
})
