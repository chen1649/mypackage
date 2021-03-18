#' T-test function
#'
#' This function conducts a t-test
#'
#' @param x Numeric vector of data.
#' @param alt Character string specifying the alternative hypothesis.
#'   This only accepts "two.sided", "less", or "greater".
#' @param m Numeric number indicating the null hypothesis value of the mean.
#'
#' @return List of elements include test statistic, degree of freedom,
#'   value of \code{alt}, numeric p-value.
#'
#' @examples
#' my_t.test((rnorm(10, mean = 0, sd = 1)), "greater", m = 0)
#'
#' @keywords prediction
#'
#' @export
my_t.test <- function(x, alt, m) {
  std_error <- (sd(x)) / sqrt(length(x))
  estimate <- mean(x)
  t <- (estimate - m) / std_error
  df <- length(x) - 1
  if (alt == "two.sided") {
    p_val <- (pt(abs(t), df, lower.tail = FALSE)) * 2
  } else if (alt == "less") {
    p_val <- pt(t, df, lower.tail = TRUE)
  } else if (alt == "greater") {
    p_val <- pt(t, df, lower.tail = FALSE)
  } else {
    stop("alternative must be 'two.sided', 'less', or 'greater'")
  }
  result <- list("test_stat" = t,
                 "df" = df,
                 "alternative" = alt,
                 "p_val" = p_val)
  return (result)
}
