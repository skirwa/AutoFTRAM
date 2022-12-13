if (!require(testthat)) install.packages('testthat')
library(dplyr)
library("testthat")
setwd('..')
source("../FTARM_SortItem.R")
data <- read.csv("../movie_ratings.csv")

test_that("To check SortItem function working",{
  expect_no_error(SortItems(data))
})

