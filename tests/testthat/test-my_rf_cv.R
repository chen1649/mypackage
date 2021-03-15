test_that("number of folds", {
  expect_type(my_rf_cv(5), "double")
})
