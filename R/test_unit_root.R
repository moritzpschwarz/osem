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

#' Automated decision based on unit root tests.
#'
#' @param urtest A named list as returned from \code{\link{test_unit_roots}}.
#' @param alpha Significance level for unit root tests.
#'
#' @return A named list as the input but with additional element
#' \code{$decision}.
#'
decide_unit_roots <- function(urtest, alpha = c("1pct", "5pct", "10pct")) {
  # validate inputs
  alpha <- match.arg(alpha)
  alpha_numeric <- switch(alpha,
    "1pct" = 0.01,
    "5pct" = 0.05,
    "10pct" = 0.1
  )

  # NOTE: think of equation
  # \Delta yt = a0 + \gamma*yt-1 + a2*t + sum(lagged Delta yt) + errort

  # unit root test in intercept+trend model
  case <- "1a"
  reject_ur <- urtest$trend@teststat["statistic", "tau3"] < urtest$trend@cval["tau3", alpha]
  if (!reject_ur) { # test whether trend was incorrectly included
    # in future, could implement test for a2 = 0 conditional on gamma = 0
    # test jointly gamma = a2 = 0 (phi3)
    trend_sign <- urtest$trend@teststat["statistic", "phi3"] > urtest$trend@cval["phi3", alpha]
    if (trend_sign) { # assume trend significant, test gamma = 0 using std. normal
      case <- "1c"
      reject_ur <- urtest$trend@teststat["statistic", "tau3"] < stats::qnorm(p = 1 - alpha_numeric, lower.tail = FALSE)
    } else { # trend was insignificant, consider simpler model
      case <- "2a"
      reject_ur <- urtest$drift@teststat["statistic", "tau2"] < urtest$drift@cval["tau2", alpha]
      if (!reject_ur) { # test whether drift was incorrectly included
        # in future, could implement test for a0 = 0 conditional on gamma = 0
        # test jointly gamma = a0 = 0 (phi1)
        cons_sign <- urtest$drift@teststat["statistic", "phi1"] > urtest$drift@cval["phi1", alpha]
        if (cons_sign) { # assume constant significant, test gamma = 0 using std. normal
          case <- "2c"
          reject_ur <- urtest$drift@teststat["statistic", "tau2"] < stats::qnorm(p = 1 - alpha_numeric, lower.tail = FALSE)
        } else { # constant was insignificant, consider simpler model
          case <- "3a"
          reject_ur <- urtest$none@teststat["statistic", "tau1"] < urtest$none@cval["tau1", alpha]
        }
      }
    }
  }
  out <- c(urtest, list(decision = list(
    alpha_ur = alpha,
    reject_ur = reject_ur,
    when = case
  )))
  return(out)
}
