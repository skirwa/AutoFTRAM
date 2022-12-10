if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../FTARM.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check minConf function working",{
  expect_equal(1,minConf(2,3,data))
})

