## code to prepare `bladderCancer` dataset

# Load package
library(tibble)

# Set sample size
n <- 1000

# Randomly sample like-variables (see https://riskcalc.org/bladderCancer/)
set.seed(123)
bladderCancer <-
  tibble(
    Gender = sample(c("M", "F"), n, replace = TRUE),
    Age = rnorm(n, 50, 10),
    RCTumorPathology = sample(c("pT0", "pTa", "pTis", "pT1", "pT2", "pT3", "pT4"), n, replace = TRUE),
    RCTumorHistology = sample(c("TCC", "SCC", "ADENOCARCINOMA"), n, replace = TRUE),
    RCTumorGrade = sample(c("High", "Low", "GX"), n, replace = TRUE),
    LymphNodeStatus = sample(c("Negative", "Positive", "NX"), n, replace = TRUE),
    DaysBetweenDXRC = rnorm(n, 500, 100),
    Recurrence = rbinom(n, 1, .5),
    Time = rweibull(n, 2, 15)
  )

# Add data to package
usethis::use_data(bladderCancer, overwrite = TRUE)
