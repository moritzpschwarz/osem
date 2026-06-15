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
  out <- c(
    urtest,
    list(
      decision = list(
        alpha_ur = alpha,
        reject_ur = reject_ur,
        when = case,

        deterministic = dplyr::case_when(
          case %in% c("1a", "1c") ~ "trend",
          case %in% c("2a", "2c") ~ "drift",
          case %in% c("3a") ~ "none"
        ),

        stationarity_type = dplyr::case_when(
          reject_ur & case %in% c("1a", "1c") ~ "trend_stationary",
          reject_ur & case %in% c("2a", "2c") ~ "drift_stationary",
          reject_ur & case %in% c("3a") ~ "level_stationary",
          TRUE ~ NA_character_
        )
      )
    )
  )
  return(out)
}

#' Provide unit root diagnostics for the data.
#'
#' @param model An [`osem`][new_osem] object.
#'
#' @return A tibble storing the unit root test results for each variable that
#' appears in a modelled equation ("d"). Since log transoformations may differ
#' by whether the variable is a dependent or independent variable, separate
#' tests are conducted. THe tibble also records in which modules the variables
#' feature.
#'
#' @export
#'
diagnostics_unit_root <- function(model) {

  # classify variables on whether need to test
  # definition modules do not need to be tested
  # LHS and RHS variables of endogenous modules need to be tested
  spec <- model$args$specification %>%
    dplyr::filter(type == "n")

  # extract variables
  dep <- spec$dependent
  dep <- trimws(unlist(strsplit(dep, ",")))
  ## there should be at most one endogenous equation per variable
  stopifnot(dplyr::n_distinct(dep) == length(dep))
  indep <- spec$independent
  indep <- strsplits(indep, splits = c("\\+"))
  indep <- gsub(" ", "", indep)
  ## a variable might appear in multiple equations as regressor
  indep <- unique(indep)

  # combine into dfs
  ur_diagnostics <- dplyr::bind_rows(
    dplyr::tibble(basevarname = dep, type = "dependent"),
    dplyr::tibble(basevarname = indep, type = "independent")
  )

  # check transformations
  ur_diagnostics <- ur_diagnostics %>%
    dplyr::mutate(transformation = dplyr::case_when(
      model$args$use_logs == "both" ~ "log",
      model$args$use_logs == "none" ~ "level",
      model$args$use_logs == "y" & type == "dependent" ~ "log",
      model$args$use_logs == "y" & type == "independent" ~ "level",
      model$args$use_logs == "x" & type == "independent" ~ "log",
      model$args$use_logs == "x" & type == "dependent" ~ "level",
      TRUE ~ "error")
    )
  if (any(ur_diagnostics$transformation == "error")) {stop("Internal error, please submit an issue on GitHub mentioning unit root diagnostics.")}

  # prepare unit root outputs
  ur_diagnostics <- ur_diagnostics %>%
    dplyr::mutate(
      ur_test = vector(mode = "list", length = dplyr::n()),
      ur_decision = NA_character_
    )

  # conduct tests
  for (i in 1:NROW(ur_diagnostics)) {
    varname <- ur_diagnostics %>% dplyr::slice(i) %>% dplyr::pull("basevarname")
    trafo <- ur_diagnostics %>% dplyr::slice(i) %>% dplyr::pull("transformation")
    stopifnot(identical(length(varname), 1L))
    stopifnot(identical(length(trafo), 1L))
    data <- model$processed_input_data %>%
      dplyr::filter(.data$na_item == varname) %>%
      dplyr::arrange(.data$time)
    if (trafo == "log") {
      data <- data %>%
        dplyr::mutate(
          values = if (any(.data$values <= 0, na.rm = TRUE)) { asinh(.data$values) } else { log(.data$values) }
        )
    } # end if log
    var_ur_test <- test_unit_roots(x = as.numeric(data$values), max.ar = model$args$max.ar, selectlags = "BIC")
    var_ur_decide <- decide_unit_roots(urtest = var_ur_test, alpha = "1pct")
    ur_diagnostics$ur_test[[i]] <- var_ur_decide
    ur_diagnostics[i, "ur_decision"] <- if (identical(var_ur_decide$decision$reject_ur, FALSE)) {
      "ur"
    } else {
      "not ur"
    }
  } # end for loop

  # find in which module(s) each variable is used
  ## initialise storage of results
  ur_diagnostics <- ur_diagnostics %>%
    dplyr::mutate(modules = vector(mode = "list", length = dplyr::n()))
  ## extract variables from specification
  ## only interested in endogenous equations
  endogenous_modules <- model$module_order %>%
    dplyr::filter(type == "n")
  ## dependent variables
  depvars_list <- lapply(X = strsplit(endogenous_modules$dependent, ","), FUN = trimws)
  ## independent variables (list of vectors, each list item corresponds to a module)
  indepvars_list <- lapply(X = strsplit(endogenous_modules$independent, "\\+"), FUN = trimws)

  ## loop through the variables and search for it as dependent or independent
  for (i in 1:NROW(ur_diagnostics)) {
    varname <- ur_diagnostics %>% dplyr::slice(i) %>% dplyr::pull("basevarname")
    type <- ur_diagnostics %>% dplyr::slice(i) %>% dplyr::pull("type")
    if (type == "dependent") {
      # search in dependent variable column
      where_found <- unlist(lapply(X = depvars_list, FUN = function(x) varname %in% x))
      modules <- endogenous_modules$index[where_found]
      stopifnot(length(modules) == 1L)
    } else if (type == "independent") {
      # search in independent variable column
      # have to ensure exact matches, in case nested names (avoid that IncomeHH could be found in RealIncomeHH)
      where_found <- unlist(lapply(X = indepvars_list, FUN = function(x) varname %in% x))
      modules <- endogenous_modules$index[where_found]
      stopifnot(length(modules) >= 1L)
    }
    ur_diagnostics$modules[[i]] <- modules
  }
  return(ur_diagnostics)

}

