# within my_lm.r
test_that("my_lm works mathematically", {
  expect_type(my_lm(mpg ~ hp, data = mtcars), "double")
  expect_s3_class(my_lm(mpg ~ hp, data = mtcars), "table")
})
test_that("not a dataframe", {
  expect_error(my_lm("a string"))
  expect_error(my_lm(123))
})
