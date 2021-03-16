test_that("number of folds", {
  expect_type(my_rf_cv(5), "double")
})

test_that("non-numeric input", {
  expect_error(my_rf_cv("input"))
})
