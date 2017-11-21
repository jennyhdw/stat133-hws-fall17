library(testthat)
source("functions.R")

context("remove missing")
test_that("remove_missing is correct",{
  expect_equal(remove_missing(c(NA,3,5,8,NA,22)),c(3,5,8,22))
  expect_equal(remove_missing(c(3,4,8,2)),c(3,4,8,2))
  expect_equal(remove_missing(c(6,5,"c",33,26)),"non-numeric argument")
})

context ("get minimum")
test_that('get_minimum is correct',{
 expect_equal(get_minimum(c(1,2,3,NA,6)),1)
 expect_equal(get_minimum(c(6,10,"b",8,12)),"non-numeric argument")
 expect_equal(get_minimum(c(6,10,50,8,12)),6)
})


context("get maximum")
test_that('get_maximum is correct',{
  expect_equal(get_maximum(c(1,2,3,NA,6)),6)
  expect_equal(get_maximum(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_maximum(c(6,10,50,8,12)),50)
})

context("get range")
test_that('get_range is correct',{
  expect_equal(get_range(c(1,2,3,NA,6)),5)
  expect_equal(get_range(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_range(c(6,10,50,8,12)),44)
})

context("get percentile10")
test_that('get_percentile10 is correct',{
  expect_equal(get_percentile10(c(1,2,3,NA,6)),1.3)
  expect_equal(get_percentile10(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_percentile10(c(6,10,50,8,12)),6.8)
})

context("get percentile90")
test_that('get_percentile90 is correct',{
  expect_equal(get_percentile90(c(1,2,3,NA,6)),5.1)
  expect_equal(get_percentile90(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_percentile90(c(6,10,50,8,12)),34.8)
})

context("get median")
test_that('get_median is correct',{
  expect_equal(get_median(c(1,2,3,NA,6)),2.5)
  expect_equal(get_median(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_median(c(6,10,50,8,12)),10)
})


context("get average")
test_that('get_average is correct',{
  expect_equal(get_average(c(1,2,3,NA,6)),mean(c(1,2,3,6)))
  expect_equal(get_average(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_average(c(6,10,50,8,12)),mean(c(6,10,50,8,12)))
})


context("get stdev")
test_that('get_stdev is correct',{
  expect_equal(get_stdev(c(1,2,3,NA,6)),sd(c(1,2,3,6)))
  expect_equal(get_stdev(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_stdev(c(6,10,50,8,12)),sd(c(6,10,50,8,12)))
})

context("get quartile1")
test_that('get_quartile1 is correct',{
  expect_equal(get_quartile1(c(1,2,3,NA,6)),1.75)
  expect_equal(get_quartile1(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_quartile1(c(6,10,50,8,12)),8)
})

context("get quartile3")
test_that('get_quartile3 is correct',{
  expect_equal(get_quartile3(c(1,2,3,NA,6)),3.75)
  expect_equal(get_quartile3(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(get_quartile3(c(6,10,50,8,12)),12)
})

context("count missing")
test_that('count_missing is correct',{
  expect_equal(count_missing(c(1,2,3,NA,6)),1)
  expect_equal(count_missing(c(6,10,"b",8,12)),"non-numeric argument")
  expect_equal(count_missing(c(6,10,50,8,12)),0)
})

context("summary_stats")
test_that('summary_stats is correct',{
  expect_equal(length(summary_stats(c(6,8,12,76))),11)
  expect_equal(summary_stats(c(6,8,12,76))[[3]],7.5)
})

#context("print_stats")
#test_that('print_stats is correct',{}

context("rescale100")
test_that("rescale100 is correct",{
  b <- c(18,15,16,4,17,9)
  
  expect_equal(rescale100(b,0,20)[1],90)
  expect_equal(rescale100(b,0,50)[1],36)
})

context("drop lowest")
test_that("drop_lowest is correct",{
  b <- c(18,15,16,4,17,9)
  c <- c(6,6,36,8,30)
  expect_equal(drop_lowest(b),c(18,15,16,17,9))
  expect_equal(drop_lowest(c),c(6,36,8,30))
})

context("score homework")
test_that("score_homework is correct",{
  hw <- c(100,80,30,70,75,85)
  expect_equal(score_homework(hw,drop = TRUE),82)
  expect_equal(score_homework(hw), mean(hw))
})

context("score quiz")
test_that("score_quiz is correct",{
  quizzes <- c(100,80,70,0)
  expect_equal(score_quiz(quizzes,drop = TRUE),mean(c(80,70,100)))
  expect_equal(score_quiz(quizzes),mean(quizzes))
})

context("score_lab")
test_that("score_lab is correct",{
  expect_equal(score_lab(12),100)
  expect_equal(score_lab(10),80)
  expect_equal(score_lab(6),0)
})
