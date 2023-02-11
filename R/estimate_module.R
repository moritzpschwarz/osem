#' Estimate the specific module using indicator saturation
#'
#' @param clean_data An input data.frame or tibble. Must be the output of clean_data() to fit all requirements.
#' @param dep_var_basename A character string of the name of the dependent variable as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param x_vars_basename A character vector of the name(s) of the independent variable(s) as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param use_logs To decide whether to log any variables. Must be one of "both", "y", or "x". Default is "both".
#' @param trend Logical. To determine whether a trend should be added. Default is TRUE.
#' @param ardl_or_ecm Either 'ardl' or 'ecm' to determine whether to estimate the model as an Autoregressive Distributed Lag Function (ardl) or as an Equilibrium Correction Model (ecm).
#' @param max.lag The maximum number of lags to use for both the AR terms as well as for the independent variables.
#' @param saturation Carry out Indicator Saturation using the 'isat' function in the 'gets' package. Needes is a character vector or string. Default is 'c("IIS","SIS")' to carry out Impulse Indicator Saturation and Step Indicator Saturation. Other possible values are 'NULL' to disable or 'TIS' or Trend Indicator Saturation. When disabled, estimation will be carried out using the 'arx' function from the 'gets' package.
#' @param saturation.tpval The target p-value of the saturation methods (e.g. SIS and IIS, see the 'isat' function in the 'gets' package). Default is 0.01.
#' @param max.block.size Integer. Maximum size of block of variables to be selected over, default = 20.
#' @param gets_selection Logical. Whether general-to-specific selection using the 'getsm' function from the 'gets' package should be done on the final saturation model. Default is TRUE.
#' @param selection.tpval Numeric. The target p-value of the model selection methods (i.e. general-to-specific modelling, see the 'getsm' function in the 'gets' package). Default is 0.01.
#'
#' @return A list containing all estimated models, with the model with the smallest BIC under 'best_model'.
#' @export
#'
#' @importFrom stats BIC coef fitted setNames
#'
#' @examples
#' sample_data <- dplyr::tibble(
#'   time = rep(seq.Date(
#'     from = as.Date("2000-01-01"),
#'     to = as.Date("2000-12-31"), by = 1
#'   ), each = 2),
#'   na_item = rep(c("yvar", "xvar"), 366), values = rnorm(366 * 2, mean = 100)
#' )
#' sample_data_clean <- clean_data(sample_data, max.lag = 4)
#' estimate_module(sample_data_clean, "yvar", "xvar")
#'
estimate_module <- function(clean_data,
                            dep_var_basename = "imports_of_goods_and_services",
                            x_vars_basename = c(
                              "gross_capital_formation",
                              "household_and_npish_final_consumption_expenditure"
                            ),
                            use_logs = c("both", "y", "x"),
                            trend = TRUE,
                            ardl_or_ecm = "ardl",
                            max.lag = 4,
                            saturation = c("IIS", "SIS"),
                            saturation.tpval = 0.01,
                            max.block.size = 20,
                            gets_selection = TRUE,
                            selection.tpval = 0.01) {
  log_opts <- match.arg(use_logs)

  if (!ardl_or_ecm %in% c("ardl", "ecm")) {
    stop("The variable 'ardl_or_ecm' in the 'estimate_module' function must be either 'ecm' or 'ardl'. You have supplied a different value.")
  }

  isat_list <- dplyr::tibble(
    ar = 0:max.lag,
    BIC = 0,
    isat_object = list(NA_complex_)
  )
  for (i in 0:max.lag) {
    if (ardl_or_ecm == "ardl") {

      # first check whether there is an x variable that is relevant (or whether it is an AR only model)
      if(!identical(x_vars_basename, character(0))){
        if (log_opts %in% c("both", "x")) {
          xvars_names <- grep("L[0-9]\\.ln",
                              grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
                              value = TRUE
          )
        } else {
          xvars_names <- grep("L[0-9]\\.",
                              grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
                              value = TRUE
          )
        }
      } else {
        # if it is an AR only model
        xvars_names <- NULL
      }

      yvar <- clean_data %>%
        dplyr::select(dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "y"), "ln.", ""), dep_var_basename))) %>%
        dplyr::pull()

      xvars <- clean_data %>%

        dplyr::select(
          if(trend){dplyr::all_of("trend")}else{NULL},
          if(!identical(x_vars_basename,character(0))){dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "x"), "ln.", ""), x_vars_basename))}else{NULL},
          if (i != 0) {
            dplyr::all_of(xvars_names[grepl(paste0("^L",1:i, collapse = "|"), xvars_names)])
          } else {
            NULL
          },
          dplyr::any_of(c("q_2", "q_3", "q_4"))
        )
    }
    if (ardl_or_ecm == "ecm") {
      yvar <- clean_data %>%
        dplyr::select(dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "y"), "D.ln.", "D."), dep_var_basename))) %>%
        dplyr::pull()


      # TO DO: Check log specification and check when model is AR only
      if(!identical(x_vars_basename, character(0))){
        xvars_names <- grep("L[0-9]\\.D.",
                            grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
                            value = TRUE
        )
      } else {
        xvars_names <- NULL
      }

      xvars <- clean_data %>%
        dplyr::select(
          dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "y"), "L1.ln.", "L1."), dep_var_basename)),
          if(!identical(x_vars_basename, character(0))){dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "x"), "L1.ln.", "L1."), x_vars_basename))}else{NULL},
          if(!identical(x_vars_basename, character(0))){dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "x"), "D.ln.", "D."), x_vars_basename))}else{NULL},
          if (i != 0) {
            dplyr::all_of(xvars_names[grepl(paste0("^L",1:i, collapse = "|"), xvars_names)])
          } else {
            NULL
          },
          dplyr::any_of(c("q_2", "q_3", "q_4"))
        )
    }

    if (!is.null(saturation)) {
      # debug_list <- list(yvar = yvar, xvars = xvars,i = i,saturation.tpval = saturation.tpval)
      # save(debug_list, file = "debug_list.RData")

      # try to prevent that isat does not run out of degrees of freedom
      if((ncol(xvars) + ((max.block.size*2)+1)) > nrow(xvars)){
        maxblocksize <- round((nrow(xvars) - ncol(xvars))/3)
      } else {maxblocksize <- max.block.size}

      if(maxblocksize < 1){
        warning(paste0("Specification not valid - the sample for estiamting the module with the dependent variable ",dep_var_basename," is not extensive enough to be estimated with lag ",i,".\n Specification skipped."))
        next
      }

      intermed.model <- gets::isat(
        y = yvar,
        mxreg = as.matrix(xvars),
        ar = if (i != 0) {
          1:i
        } else {
          NULL
        },
        plot = FALSE,
        print.searchinfo = FALSE,
        iis = TRUE,
        sis = TRUE,
        t.pval = saturation.tpval,
        max.block.size = maxblocksize
      )
    } else {
      intermed.model <- gets::arx(
        y = yvar,
        mxreg = as.matrix(xvars),
        ar = if (i != 0) {
          1:i
        } else {
          NULL
        },
        plot = FALSE
      )
    }


    isat_list[i + 1, "BIC"] <- stats::BIC(intermed.model)
    isat_list[i + 1, "isat_object"] <- dplyr::tibble(isat_object = list(intermed.model))
  }


  best_isat_model <- isat_list %>%
    dplyr::filter(BIC == min(dplyr::all_of("BIC"))) %>%
    dplyr::pull(dplyr::all_of("isat_object")) %>%
    dplyr::first()

  ## gets selection on the best model ------------
  best_isat_model.selected <- gets::gets.isat(best_isat_model,
                                              print.searchinfo = FALSE,
                                              t.pval = selection.tpval)

  retained.coefs <- row.names(best_isat_model.selected$mean.results)
  retained.coefs <- retained.coefs[!grepl("^mconst|^sis[0-9]+|^iis[0-9]+|^ar[0-9]+", retained.coefs)]
  retained.xvars <- as.matrix(xvars[,retained.coefs])

  retained.xvars <- if (ncol(retained.xvars) > 0) {retained.xvars} else {NULL}

  best_isat_model.selected.isat <- gets::isat(y = yvar,
                                              ar = best_isat_model$aux$args$ar,
                                              mc = best_isat_model$aux$args$mc,
                                              mxreg = retained.xvars,
                                              plot = FALSE,
                                              print.searchinfo = FALSE,
                                              iis = TRUE,
                                              sis = TRUE,
                                              t.pval = saturation.tpval)


  out <- list()
  out$isat_list <- isat_list
  #out$best_model <- isat_list %>%
  #  dplyr::filter(BIC == min(BIC)) %>%
  #  dplyr::pull(dplyr::all_of("isat_object")) %>%
  #  dplyr::first()
  out$best_model <- best_isat_model.selected.isat
  out$args <- list(clean_data = clean_data,
                   dep_var_basename = dep_var_basename,
                   x_vars_basename = x_vars_basename,
                   use_logs = match.arg(use_logs),
                   ardl_or_ecm = ardl_or_ecm,
                   max.lag = ardl_or_ecm)

  return(out)
}
