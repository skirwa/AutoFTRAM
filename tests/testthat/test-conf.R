if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../FTARM_SupAndConf.R")
source("../FTARM_TIDFunctions.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check conf function working",{
  expect_equal(0.111,conf(1,2,data))
  expect_equal(0,conf(2,5,data))
  expect_equal( 0.333,conf(1,3,data))
})

