if (!require(testthat)) install.packages('testthat')

library("testthat")
setwd('..')
source("../FTARM.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check minSup function working",{
  expect_equal(0.45,minSup(1,data))
  
})

