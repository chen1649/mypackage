#' K-nearest neighbors cross-validation function
#'
#' This function predicts output class using covariates.
#'
#' @param train Dataframe to be inputted
#' @param cl Class value of training data
#' @param k_nn Integer represemmting number of neighbors
#' @param k_cv Integer representing the number of folds
#'
#' @return a list with objects class and cv_err. Class is a vector of predicted
#'   class. CV_err is a numeric with cross-validation misclassification error
#'
#' @examples
#' penguins <- mypackage::my_penguins
#' data <- penguins %>% tidyr::drop_na()
#' train <- data[3:6]
#' cl <- data$species
#' my_knn_cv(train, cl, 1, 5)
#'
#' @keywords prediction
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
  fold <- sample(rep(1:k_cv, length = length(cl)))
  my_matrix <- matrix(NA, length(cl))
  error = rep(NA, k_cv)
  for (i in 1:k_cv) {
    x_train <- train[fold != i, ]
    y_train <- cl[fold != i]
    x_test <- train[fold == i, ]
    y_test <- cl[fold == i]
    prediction <- knn(x_train, x_test, y_train, k = k_nn)
    error[i] <- mean(y_test != prediction)
  }
  class <- knn(train, train, cl, k = k_nn)
  cv_err <- mean(error)
  return(list("class" = class, "cv_err" = cv_err))
}
