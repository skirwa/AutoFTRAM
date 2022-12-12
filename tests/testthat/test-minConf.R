if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
print(getwd())
source("../FTARM_TIDFunctions.R")
source("../FTARM_SupAndConf.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check minConf function working",{
  expect_equal(0.111,minConf(data,1,2))
  expect_equal(0.333,minConf(data,1,3))
  expect_equal(0.25,minConf(data,2,3))
})

