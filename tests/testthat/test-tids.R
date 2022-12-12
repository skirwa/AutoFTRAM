if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../FTARM_TIDFunctions.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check tids function working",{
  expect_equal(9,tids(1,data))
  expect_equal(3,tids(3,data))
  expect_equal(1,tids(5,data))
})
  
