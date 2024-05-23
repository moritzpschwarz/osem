#' Estimate the specific module using indicator saturation
#'
#' @param clean_data An input data.frame or tibble. Must be the output of clean_data() to fit all requirements.
#' @param dep_var_basename A character string of the name of the dependent variable as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param x_vars_basename A character vector of the name(s) of the independent variable(s) as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param use_logs To decide whether to log any variables. Must be one of "both", "y", or "x". Default is "both".
#' @param trend Logical. To determine whether a trend should be added. Default is TRUE.
#' @param ardl_or_ecm Either 'ardl' or 'ecm' to determine whether to estimate the model as an Autoregressive Distributed Lag Function (ardl) or as an Equilibrium Correction Model (ecm).
#' @param max.ar Integer. The maximum number of lags to use for the AR terms. as well as for the independent variables.
#' @param max.dl Integer. The maximum number of lags to use for the independent variables (the distributed lags).
#' @param saturation Carry out Indicator Saturation using the 'isat' function in the 'gets' package. Needes is a character vector or string. Default is 'c("IIS","SIS")' to carry out Impulse Indicator Saturation and Step Indicator Saturation. Other possible values are 'NULL' to disable or 'TIS' or Trend Indicator Saturation. When disabled, estimation will be carried out using the 'arx' function from the 'gets' package.
#' @param saturation.tpval The target p-value of the saturation methods (e.g. SIS and IIS, see the 'isat' function in the 'gets' package). Default is 0.01.
#' @param max.block.size Integer. Maximum size of block of variables to be selected over, default = 20.
#' @param gets_selection Logical. Whether general-to-specific selection using the 'getsm' function from the 'gets' package should be done on the final saturation model. Default is TRUE.
#' @param selection.tpval Numeric. The target p-value of the model selection methods (i.e. general-to-specific modelling, see the 'getsm' function in the 'gets' package). Default is 0.01.
#'
#' @return A list containing all estimated models, with the model with the smallest BIC under 'best_model'.
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
#' sample_data_clean <- aggregate.model:::clean_data(sample_data, max.ar = 4)
#' aggregate.model:::estimate_module(sample_data_clean, "yvar", "xvar")
#'
estimate_module <- function(clean_data,
                            dep_var_basename,
                            x_vars_basename,
                            use_logs = c("both", "y", "x"),
                            trend = TRUE,
                            ardl_or_ecm = "ardl",
                            max.ar = 4,
                            max.dl = 2,
                            saturation = c("IIS", "SIS"),
                            saturation.tpval = 0.01,
                            max.block.size = 20,
                            gets_selection = TRUE,
                            selection.tpval = 0.01) {


  # Set-up ------------------------------------------------------------------


  log_opts <- match.arg(use_logs)

  if (!ardl_or_ecm %in% c("ardl", "ecm")) {
    stop("The variable 'ardl_or_ecm' in the 'estimate_module' function must be either 'ecm' or 'ardl'. You have supplied a different value.")
  }

  isat_list <- dplyr::tibble(
    ar = 0:max.ar,
    BIC = 0,
    isat_object = list(NA_complex_)
  )
  for (i in 0:max.dl) {
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

      if(i == 0){
        xvars_initial <- xvars
      }

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
      if(i == 0){
        xvars_initial <- xvars
      }
    }

    # ISAT modelling ----------------------------------------------------------

    if (!is.null(saturation)) {
      # debug_list <- list(yvar = yvar, xvars = xvars,i = i,saturation.tpval = saturation.tpval)
      # save(debug_list, file = "debug_list.RData")

      # try to prevent that isat does not run out of degrees of freedom
      if((ncol(xvars) + ((max.block.size*2)+1)) > nrow(xvars)){
        maxblocksize <- round((nrow(xvars) - ncol(xvars))/3)
      } else {maxblocksize <- max.block.size}

      if(maxblocksize < 1){
        warning(paste0("Specification not valid - the sample for estimating the module with the dependent variable ",dep_var_basename," is not extensive enough to be estimated with lag ",i,".\n Specification skipped."))
        next
      }

      if(tidyr::drop_na(cbind(yvar,xvars)) %>% nrow == 0){
        next
      }

      try(intermed.model <- gets::isat(
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
      ), silent = TRUE)
    } else {

      # ARX Modelling -----------------------------------------------------------
      # Save original arx mc warning setting and disable it here
      tmpmc <- options("mc.warning")
      on.exit(options(tmpmc)) # set the old mc warning on exit

      options(mc.warning = FALSE)


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


    isat_list[i + 1, "BIC"] <- if(exists("intermed.model")){stats::BIC(intermed.model)}else{NA}
    isat_list[i + 1, "isat_object"] <- if(exists("intermed.model")){dplyr::tibble(isat_object = list(intermed.model))}else{NA}
    if(exists("intermed.model")){rm(intermed.model)}
  }

  if (all(is.na(isat_list$BIC))){
    dplyr::tibble(time = clean_data$time,
                  y = yvar,
                  xvars_initial) %>%
      dplyr::rename_with(.cols = "y",.fn = ~paste0(ifelse(log_opts %in% c("both", "y"), "D.ln.", "D."), dep_var_basename)) %>%
      dplyr::select(-c(any_of(c("q_1", "q_2", "q_3", "q_4", "trend")))) %>%
      tidyr::pivot_longer(-c("time")) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$value, color = .data$name)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::facet_wrap(~.data$name, scales = "free_y", ncol = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") -> p
    print(p)

    stop(paste0("No model could be estimated for the module for ",dep_var_basename,
                ".\n Check the equation set-up and the data. Check also if there are missing variables that might lead to an empty sample.\n",
                "For debugging, a plot for this module has been produced - check if there are enough overlapping sample periods."))
  }

  best_isat_model <- isat_list %>%
    dplyr::filter(BIC == min(isat_list$BIC, na.rm = TRUE)) %>%
    dplyr::pull(dplyr::all_of("isat_object")) %>%
    dplyr::first()

  # gets selection on the best model ----------------------------------------


  if(gets_selection){
    best_isat_model.selected <- gets::gets(best_isat_model,
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
  }


  # Output ------------------------------------------------------------------
  out <- list()
  out$isat_list <- isat_list
  #out$best_model <- isat_list %>%
  #  dplyr::filter(BIC == min(BIC)) %>%
  #  dplyr::pull(dplyr::all_of("isat_object")) %>%
  #  dplyr::first()
  out$best_model <- if(gets_selection){best_isat_model.selected.isat} else {best_isat_model}

  out$args <- list(clean_data = clean_data,
                   dep_var_basename = dep_var_basename,
                   x_vars_basename = x_vars_basename,
                   use_logs = match.arg(use_logs),
                   ardl_or_ecm = ardl_or_ecm,
                   max.ar = max.ar,
                   max.dl = max.dl)

  return(out)
}
