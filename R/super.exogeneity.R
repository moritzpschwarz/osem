#' Super Exogeneity (Parameter Invariance) Test
#'
#' This function tests for super exogeneity (parameter invariance) of the model. Parameter invariance is a key component of Super Exogeneity, which
#' was first introduced in Engle, Hendry, and Richard (1983; Econometrica). This test runs an indicator saturation model for each independent variable
#' that is present in the \code{initial.model} object - these individual models are called the marginal models.
#' If any outliers or step-shifts are detected using the \code{isat} function from the \code{gets} package in the marginal models,
#' then these indicators are added to the initial model, called the conditional model.
#' Any pre-existing indicators from the initial model are removed, as otherwise the power of the test would be reduced.
#' The conditional model is then used to run a linear regression
#' and to obtain an F-Stat statistic to determine whether the shocks detected in the marginal models affect the conditional model.
#'
#'
#' @param initial.model An object of class \code{isat} (i.e. from the \code{gets} package) that contains the initial model
#' @inheritParams estimate_module
#' @return An h-test object with the results of the super exogeneity test
#' @references Engle, R. F., Hendry, D. F., & Richard, J. F. (1983). Exogeneity. Econometrica: Journal of the Econometric Society, 73-85.
#' @references Engle, R. F., & Hendry, D. F. (1993). Testing superexogeneity and invariance in regression models. Journal of Econometrics, 56(1-2), 119-139.
#' @references Hendry, D. F., & Santos, C., 'An Automatic Test of Super Exogeneity', in Bollerslev, T., Russell, J., & Watson, M. (Eds.). (2010). Volatility and time series econometrics: essays in honor of Robert Engle. OUP oxford.
#' @references Castle, J. L., Hendry, D. F., & Martinez, A. B. (2017). Evaluating forecasts, narratives and policy using a test of invariance. Econometrics, 5(3), 39.
#' @export
#'
#'
#' @examples
#' #load Hoover and Perez (1999) data:
#' data(hpdata, package = "gets")
#'
#' ##run isat with step impulse saturation on two lags and a constant 1 percent significance level:
#' is.model <- gets::isat(
#'   y = hpdata$GCQ,
#'   mxreg = hpdata[,"GYDQ", drop = FALSE],
#'   ar = 1:2,
#'   sis = TRUE,
#'   t.pval = 0.01
#' )
#'
#' super.exogeneity(is.model)

super.exogeneity <- function(initial.model, saturation.tpval = 0.01, quiet = FALSE) {

  # check if the model is an isat or an arx object
  stopifnot(inherits(initial.model, "isat") | inherits(initial.model, "arx") | inherits(initial.model, "gets"))

  xvars_to_test <- setdiff(row.names(initial.model$mean.results),c("mconst","trend",initial.model$ISnames, "q_1","q_2","q_3","q_4"))
  xvars_to_test <- xvars_to_test[!grepl("^ar[0-9]+$",xvars_to_test)]
  # find unique xvariables when removing lags that start with L
  xvars_to_test_unique <- unique(gsub("L[0-9]+\\.","",xvars_to_test))

  collected_indicators <- dplyr::tibble(.rows = length(initial.model$aux$y))
  testable_vars <- c()
  marginal.models <- list()
  for(xvar in xvars_to_test_unique){
    # xvar = xvars_to_test_unique[2]

    # get all variables in xvars_to_test for which xvar is the base variable (so some might start with L for lags)
    xvars_cur <- grep(xvar,xvars_to_test,value = TRUE)

    # if any of the xvars_cur is in the xvars_to_test_unique then take that as the dependent variable
    # if not then take the one with the lowest lag as the dependent variable
    if(any(xvars_cur %in% xvars_to_test_unique)){
      dep_var_exog_test <- xvars_cur[xvars_cur %in% xvars_to_test_unique]
    } else {
      # find the variable with the lowest lag
      # grep only the numbers of the strings in xvars_cur in the starting L[0-9]+.
      # then convert to numeric and find the minimum
      dep_var_exog_test <- xvars_cur[which.min(as.numeric(gsub("L([0-9]+)\\..*","\\1",xvars_cur)))]
    }

    # the dep_var_exog_test as the dependent variable and all other variables as independent variables
    xvars_ready <- setdiff(xvars_cur,dep_var_exog_test)
    xvars_ready_df <- if(!identical(xvars_ready, character(0))) {initial.model$aux$mX[,xvars_ready, drop = FALSE]} else {NULL}

    gets::isat(y = initial.model$aux$mX[,dep_var_exog_test],
               mxreg = xvars_ready_df,
               iis = TRUE,
               sis = TRUE,
               plot = FALSE,
               print.searchinfo = FALSE,
               t.pval = saturation.tpval) -> isat_test

    # add isat_test to the list that is called "marginal.models" as a new element (append it to the back)
    marginal.models <- c(marginal.models, list(isat_test))


    # indicate whether this variable is even testable (i.e. if an indicator is found)
    if(is.null(isat_test$ISnames)) {
      testable_vars_cur <- FALSE
      names(testable_vars_cur) <- dep_var_exog_test
      testable_vars <- c(testable_vars, testable_vars_cur)
    } else {
      testable_vars_cur <- TRUE
      names(testable_vars_cur) <- dep_var_exog_test
      testable_vars <- c(testable_vars, testable_vars_cur)
    }

    if(any(colnames(isat_test$aux$mX) %in% names(collected_indicators))){
      collected_indicators <- dplyr::bind_cols(collected_indicators,
                                               isat_test$aux$mX %>%
                                                 dplyr::as_tibble() %>%
                                                 dplyr::select(-dplyr::any_of(names(collected_indicators))))
    } else {
      collected_indicators <- dplyr::bind_cols(collected_indicators, isat_test$aux$mX)
    }
  }

  names(marginal.models) <- xvars_to_test_unique

  if(any(testable_vars)) {

    data_for_test <- dplyr::bind_cols(y = initial.model$aux$y,
                                      initial.model$aux$mX[,colnames(initial.model$aux$mX) %in% xvars_to_test_unique, drop = FALSE],
                                      collected_indicators)

    testmodel <- stats::lm(y ~ . -1, data = data_for_test)

    varnames_to_test <- names(testmodel$coefficients)[!names(testmodel$coefficients) %in% c("mconst", xvars_to_test)]

    b <- as.numeric(t(testmodel$coefficients[names(testmodel$coefficients) %in% varnames_to_test]))

    vcov_matrix <- stats::vcov(testmodel)[row.names(stats::vcov(testmodel)) %in% varnames_to_test, colnames(stats::vcov(testmodel)) %in% varnames_to_test] %>%
      solve() %>%
      as.matrix()

    fstat <- b %*% vcov_matrix %*% t(t(b))/2

    fstat_pvalue <- stats::pf(fstat,
                       df1 = nrow(as.data.frame(testmodel$coefficients[varnames_to_test])),
                       df2 = nrow(as.data.frame(testmodel$coefficients)) - nrow(as.data.frame(testmodel$coefficients[varnames_to_test])),
                       lower.tail = FALSE)

    chisq_pvalue <- 1 - stats::pchisq(fstat*2, nrow(as.data.frame(initial.model$coefficients[varnames_to_test])))

    out <- list()
    out$statistic <- c("F-Stat" = fstat)
    out$p.value <- fstat_pvalue
    out$alternative <- "Parameter Variance"
    out$method <- "Super Exogeneity (Parameter Invariance) Test"
    #out$parameter <- c(`p-value using chi-squared` = chisq_pvalue)
    out$parameter <- c(df1 = nrow(as.data.frame(testmodel$coefficients[varnames_to_test])),
                       df2 = nrow(as.data.frame(testmodel$coefficients)) - nrow(as.data.frame(testmodel$coefficients[varnames_to_test])))
    out$data.name <- paste0("Conditional Model for: ",initial.model$aux$y.name) # , "\nTesting for: ")
    out$model.args <- list(testdata = data_for_test,
                           conditional.model = testmodel,
                           marginal.models = marginal.models,
                           saturation.tpval = saturation.tpval,
                           initial.isat.model = initial.model)

    class(out) <- "htest"

    return(out)

    #fstat <- b %*% as.matrix(solve(vcov(model)[-c(1:3), -c(1:3)])) %*% t(t(b))/2
    # fstat_pvalue <- pf(fstat,
    #                    df1 = nrow(as.data.frame(model$coefficients[-c(1:3)])),
    #                    df2 = nrow(as.data.frame(model$coefficients)) - nrow(as.data.frame(model$coefficients[-c(1:3)])),
    #                    lower.tail = FALSE)
    #
    # chisq_pvalue <- 1 - pchisq(fstat*2, nrow(as.data.frame(model$coefficients[-c(1:3)])))



    # cat("Super exogneity test (Null hyphothesis is parameter invariance):","\n")
    # cat("F-statistic:", fstat, "\n")
    # cat("F-distribution p-value:", fstat_pvalue, "\n")
    # cat("Chi-2 Distribution p-value:", chisq_pvalue, "\n")

  } else {
    if(!quiet){cat(paste0("No Outliers or Step-Shifts detected in the marginal equations to test for Super Exogeneity in ",initial.model$aux$y.name,".\nHence not possible to run the test.","\n"))}
    return(NA)
  }
}
