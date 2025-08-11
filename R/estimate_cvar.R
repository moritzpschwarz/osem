#' Estimate the specific CVAR sub-system using the Johansen procedure
#'
#' @param clean_data An input data.frame or tibble. Must be the output of
#' clean_data() to fit all requirements.
#' @param system_name A character string specifying the name of the CVAR
#' sub-system.
#' @param dep_vars_basename A character string of the names of the dependent
#' variable separated by a comma, and contained in clean_data() in a level form
#' (i.e. no ln or D in front of the name).
#' @param x_vars_basename A character vector of the name(s) of the independent
#' variable(s), and contained in clean_data() in a level form (i.e. no ln or D
#' in front of the name). Can be an empty string if no exogenous regressors are
#' included.
#' @param use_logs To decide whether to log any variables. Must be one of
#' 'both', 'y', 'x', or 'none'. Default is 'both'.
#' @param cvar.ar Number of lags of the VAR system in levels. Must be > 2.
#' @param freq Data frequency if want to include seasonal dummies, NULL
#' otherwise.
#' @param coint_deterministic Character specifying whether and if yes which
#' deterministic component to include in the cointegrating relationship.
#' @param coint_significance Significance level for the rank test for
#' cointegration. Can only be one of “1pct”, “5pct”, “10pct”.
#'
#' @inheritParams run_model
#'
#' @return A list containing unit root tests for the variables involved, the
#' resulting CVAR model, and the argument setting:
#' \item{cointtest}{An object of \link[urca]{ca.jo-class} with the
#' cointegration test result.}
#' \item{vecm}{A list storing the restricted vector error correction model as
#' returned by [urca::cajorls()].}
#' \item{varm}{An object storing the restricted vector autoregressive model in
#' levels as returned by [vars::vec2var()].}
#' \item{rank}{The CVAR rank as determined by Johansen's trace test.}
#' \item{urtest}{A list of two lists storing the augmented Dickey-Fuller unit
#' root tests for all x and y variables as class \link[urca]{ur.df-class}.}
#' \item{args}{A list storing the settings of the function call.}

estimate_cvar <- function(clean_data, system_name, dep_vars_basename,
                          x_vars_basename, use_logs, cvar.ar, freq,
                          coint_deterministic = c("none", "const", "trend"),
                          coint_significance = c("1pct", "5pct", "10pct"),
                          quiet = FALSE) {
  # input validation
  coint_significance <- match.arg(coint_significance)
  coint_deterministic <- match.arg(coint_deterministic)

  y.basenames <- trimws(unlist(strsplit(dep_vars_basename, split = ",")))
  if (!(length(y.basenames) > 1)) {
    stop("This is not CVAR sub-system. Only one dependent variable specified.")
  }
  y.names <- paste0(ifelse(use_logs %in% c("both", "y"), "ln.", ""), y.basenames)
  yvars <- clean_data %>%
    dplyr::select(dplyr::all_of(y.names))

  if (identical(x_vars_basename, character(0))) { # no RHS vars specified
    x.names <- character(0)
    xvars <- NULL
  } else {
    x.names <- paste0(ifelse(use_logs %in% c("both", "x"), "ln.", ""), x_vars_basename)
    xvars <- clean_data %>%
      dplyr::select(dplyr::all_of(x.names))
    # unlike for estimate_module, no need to pick lags specifically as regressors; ca.jo() handles it
  }

  # make unit root tests for x and y variables
  y_urtest <- vector("list", length(y.names))
  names(y_urtest) <- y.names
  x_urtest <- vector("list", length(x.names))
  names(x_urtest) <- x.names

  # run all types of ADF tests
  for (i in seq_along(y.names)) {
    y_urtest[[i]] <- test_unit_roots(x = yvars %>% dplyr::pull(y.names[i]), max.ar = cvar.ar, selectlags = "BIC") %>%
      decide_unit_roots(alpha = "1pct")
  }
  for (i in seq_along(x.names)) { # will be skipped if length(x.names) == 0L
    x_urtest[[i]] <- test_unit_roots(x = xvars %>% dplyr::pull(x.names[i]), max.ar = cvar.ar, selectlags = "BIC") %>%
      decide_unit_roots(alpha = "1pct")
  }

  # check that all CVAR elements are unit roots
  y_urreject <- purrr::map_lgl(y_urtest, ~ .$decision$reject_ur)
  if (length(y_urreject > 0) & any(y_urreject)) {
    stop(paste0("Not all CVAR variables in CVAR sub-system ", system_name, " are I(1). For the following variables, we reject the unit root hypothesis: ", names(y_urreject[y_urreject]), "."))
  }
  # ...and all exogenous regressors stationary
  x_urreject <- purrr::map_lgl(x_urtest, ~ .$decision$reject_ur)
  if (length(x_urreject > 0) & !all(x_urreject)) { # only check if have X vars
    stop(paste0("Not all exogenous regressors in CVAR sub-system ", system_name, " are stationary. For the following variables, we do not reject the unit root hypothesis: ", names(x_urreject[!x_urreject]), "."))
  }

  # cointegration test
  cointtest <- urca::ca.jo(
    x = yvars, type = "trace", ecdet = coint_deterministic, K = cvar.ar,
    season = freq, dumvar = xvars, spec = "transitory"
  )
  # automated rank selection
  coint_cv <- cointtest@cval
  coint_stat <- cointtest@teststat
  ## extract the applicable critical values
  coint_cv <- coint_cv[, coint_significance]
  ## which do we reject?
  which_reject <- coint_stat > coint_cv
  ## determine the first one we don't reject, starting from lowest r test
  first_pass <- utils::tail(which(which_reject == FALSE), 1)
  # communicate outcome
  ## which_reject was never FALSE -> rejected all tests
  if (identical(first_pass, integer(0))) {
    stop("All trace tests rejected. Specified variables are likely already stationary. Please re-specify the model.")
    ## which_reject was already FALSE for the last element, r = 0 -> no cointegration
  } else if (identical(as.integer(first_pass), length(which_reject))) {
    # another way to check: name if first_pass element -> in this part of the else-if should always hold
    if (!identical(names(first_pass), "r = 0  |")) {
      stop("Internal error. Please report as an issue on GitHub.")
    }
    stop("Cannot reject r = 0. Specified variables seem nonstationary but not cointegrated. Please re-specify the model.")
  } else {
    rankval <- length(which_reject) - first_pass %>% as.integer()
    if (!quiet) {
      message(paste0("Determined cointegration rank for CVAR sub-system '", system_name, "' was ", rankval, ". Proceed to estimate CVAR."))
    }
  }

  # estimated the restricted model
  vecm <- urca::cajorls(z = cointtest, r = rankval)
  varm <- vars::vec2var(z = cointtest, r = rankval)

  # return object
  out <- list()
  out$cointtest <- cointtest
  out$vecm <- vecm
  out$varm <- varm
  out$rank <- rankval
  out$urtest <- list(
    y_urtest = y_urtest,
    x_urtest = x_urtest
  )
  out$args <- list(
    clean_data = clean_data,
    cvar = system_name,
    dep_vars_basename = dep_vars_basename,
    x_vars_basename = x_vars_basename,
    use_logs = use_logs,
    ar = cvar.ar,
    coint_deterministic = coint_deterministic,
    coint_significance = coint_significance
  )
  return(out)
}
