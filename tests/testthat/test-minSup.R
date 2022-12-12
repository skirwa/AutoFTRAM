if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../FTARM_TIDFunctions.R")
source("../FTARM_SupAndConf.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check minSup function working",{
  expect_equal(0.05,minSup(1,2,data))
  expect_equal(0.15,minSup(1,3,data))
  expect_equal(0,minSup(2,4,data))
})

