if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../R/FTARM_SupAndConf.R")
source("../R/FTARM_TIDFunctions.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check Sup function working",{
  expect_equal(0.2,sup(2,data))
  expect_equal(0.05,sup(6,data))
  expect_equal(0,sup(4,data))
})

