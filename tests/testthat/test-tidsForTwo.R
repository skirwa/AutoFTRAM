if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../R/FTARM_TIDFunctions.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check tidsForTwo function working",{
  expect_equal(3,tidsForTwo(1,3,data))
  expect_equal(0,tidsForTwo(2,4,data))
  expect_equal(1,tidsForTwo(3,5,data))
})
