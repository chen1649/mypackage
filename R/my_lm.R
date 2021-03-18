#' Linear model function
#'
#' This function fits a linear model.
#'
#' @param formula A formula class object
#' @param data Dataframe to be inputted
#'
#' @return Table with rows for each coefficient (including the (Intercept)!)
#'   and columns for the Estimate, Std. Error, t value, and Pr(>|t|).
#'
#' @examples
#' my_lm(mpg ~ hp, data = mtcars)
#'
#' @keywords prediction inference
#'
#' @export
my_lm <- function(formula, data) {
  x <- model.matrix(formula, data)
  mdl_frame <- model.frame(formula, data)
  y <- model.response(mdl_frame)
  coefficient <- solve((t(x) %*% x)) %*% t(x) %*% y
  df <- nrow(data) - ncol(x)
  variance_formula <- ((y - (x %*%  coefficient))^2) / df
  variance <- sum(variance_formula)
  std_error <- sqrt(diag(variance * solve((t(x) %*% x))))
  t <- coefficient / std_error
  p_val <- (pt(abs(t), df, lower.tail = FALSE)) * 2
  results <- cbind(coefficient, std_error, t, p_val)
  colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(as.table(results))
}
