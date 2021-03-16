test_that("output is a list", {
  data <- penguins %>% drop_na()
  train <- data[3:6]
  cl <- data %>% pull(species)
  expect_type(my_knn_cv(train, cl, 1, 5), "list")
})

test_that("input is not dataframe", {
  expect_error(my_knn_cv("train", "cl", 1, 5))
})
