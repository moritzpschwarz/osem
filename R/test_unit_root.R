#' Run three types of ADF unit root tests.
#'
#' @param x A numeric vector.
#' @param max.ar Maximum lag for augmented Dickey-Fuller test.
#' @param selectlags Character specifying "Fixed" lags or automatic lag
#' selection with information criteria "AIC" or "BIC".
#'
#' @return A named list with four elements, storing the function arguments and
#' the resulting three ADF unit root tests: (i) no deterministic terms, (ii) an
#' intercept, (iii) intercept and trend.
#'
test_unit_roots <- function(x, max.ar, selectlags = c("Fixed", "AIC", "BIC")) {
  # input validation
  selectlags <- match.arg(selectlags)

  # loop over deterministic terms
  dets <- c("none", "drift", "trend")
  out <- sapply(X = dets, FUN = urca::ur.df, y = x, lags = max.ar, selectlags = selectlags)
  out <- c(list(args = list(selectlags = selectlags, max.ar = max.ar)), out)

  # return
  return(out)
}
