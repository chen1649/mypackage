#' K-nearest neighbors cross-validation function
#'
#' This function predicts output class using covariates.
#'
#' @param k Dataframe to be inputted
#'
#' @return the number of mse errors
#' @importFrom stats model.frame model.matrix model.response predict pt sd
#' @importFrom class knn
#' @importFrom randomForest randomForest
#' @importFrom tidyr drop_na
#' @importFrom dplyr filter
#'
#' @export
my_rf_cv <- function(k) {
  penguins <- mypackage::my_penguins
  penguins <- penguins %>% tidyr::drop_na()
  fold <- sample(rep(1:k, length = nrow(penguins)))
  data <- penguins
  MSE_list <- rep(NA, k)
  for (i in 1:k) {
    train_data <- data[fold != i,]
    test_data <- data[fold == i,]
    MODEL <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                            flipper_length_mm, data = train_data, ntree = 100)
    PREDICTIONS <- predict(MODEL, test_data[, -1])
    MSE_list[i] <- mean((PREDICTIONS - test_data$body_mass_g)^2)
  }
  MSE <- mean(MSE_list)
  return(MSE)
}
