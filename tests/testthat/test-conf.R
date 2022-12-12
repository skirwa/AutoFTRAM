if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../FTARM_SupAndConf.R")
source("../FTARM_TIDFunctions.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check conf function working",{
  expect_equal(0.3333333,conf(data,1,3))
  expect_equal(0,conf(data,2,5))
  expect_equal( 0.1111111,conf(data,1,6))
})

