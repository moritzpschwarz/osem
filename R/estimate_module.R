#' Estimate the specific module using indicator saturation
#'
#' @param clean_data An input data.frame or tibble. Must be the output of
#' clean_data() to fit all requirements.
#' @param dep_var_basename A character string of the name of the dependent
#' variable as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param x_vars_basename A character vector of the name(s) of the independent
#' variable(s) as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param use_logs To decide whether to log any variables. Must be one of
#' 'both', 'y', 'x', or 'none'. Default is 'both'.
#' @param trend Logical. To determine whether a trend should be added.
#' Default is TRUE.
#' @param ardl_or_ecm Either 'ardl' or 'ecm' to determine whether to estimate
#' the model as an Autoregressive Distributed Lag Function (ardl) or as an
#' Equilibrium Correction Model (ecm).
#' @param max.ar Integer. The maximum number of lags to use for the AR terms.
#' as well as for the independent variables.
#' @param max.dl Integer. The maximum number of lags to use for the independent
#'  variables (the distributed lags).
#' @param saturation Carry out Indicator Saturation using the 'isat' function
#' in the 'gets' package. Needs a character vector or string. Default is
#' 'c("IIS","SIS")' to carry out Impulse Indicator Saturation and Step Indicator
#' Saturation. Other possible values are 'NULL' to disable or 'TIS' or Trend
#' Indicator Saturation. When disabled, estimation will be carried out using
#' the 'arx' function from the 'gets' package.
#' @param saturation.tpval The target p-value of the saturation methods (e.g.
#' SIS and IIS, see the 'isat' function in the 'gets' package). Default is 0.01.
#' @param max.block.size Integer. Maximum size of block of variables to be
#' selected over, default = 20.
#' @param gets_selection Logical. Whether general-to-specific selection using
#' the 'getsm' function from the 'gets' package should be done on the final
#' saturation model. Default is TRUE.
#' @param selection.tpval Numeric. The target p-value of the model selection
#' methods (i.e. general-to-specific modelling, see the 'getsm' function
#' in the 'gets' package). Default is 0.01.
#' @inheritParams forecast_model
#' @inheritParams run_module
#' @inheritParams run_model
#'
#' @return A list containing all estimated models, with the model with the smallest BIC under 'best_model'.
#'
#' @importFrom stats BIC coef fitted setNames
estimate_module <- function(clean_data,
                            dep_var_basename,
                            x_vars_basename,
                            use_logs = "both",
                            trend = TRUE,
                            ardl_or_ecm = "ardl",
                            max.ar = 4,
                            max.dl = 2,
                            saturation = c("IIS", "SIS"),
                            saturation.tpval = 0.01,
                            max.block.size = 20,
                            gets_selection = TRUE,
                            selection.tpval = 0.01,
                            keep,
                            pretest_steps,
                            saturation_only_post_selection,
                            manual_models,
                            quiet = FALSE) {
  # Set-up ------------------------------------------------------------------
  log_opts <- use_logs

  if (!ardl_or_ecm %in% c("ardl", "ecm")) {
    stop("The variable 'ardl_or_ecm' in the 'estimate_module()' or the 'run_model()' function must be either 'ecm' or 'ardl'. You have supplied a different value.")
  }

  isat_list <- dplyr::tibble(
    ar = 0:max.ar,
    BIC = 0,
    isat_object = list(NA_complex_),
    AR = NA,
    ARCH = NA
  )
  for (i in 0:max.dl) {
    if (ardl_or_ecm == "ardl") {
      # first check whether there is an x variable that is relevant (or whether it is an AR only model)
      if(!identical(x_vars_basename, character(0))){

        # grab all starting with a lag (the contemporaneous are added below)
        xvars_names <- grep("^L[0-9]\\.",
                            grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
                            value = TRUE)
        # remove all differences
        xvars_names <- xvars_names[!grepl("^L[0-9]\\.D\\.",xvars_names)]

        # check log specification
        if (log_opts %in% c("y", "none")) {
          # remove all log variables
          xvars_names <- xvars_names[!grepl("ln\\.",xvars_names)]
        } else {
          # grab only the log variables
          xvars_names <- xvars_names[grepl("ln\\.",xvars_names)]
        }
      } else {
        # if it is an AR only model
        xvars_names <- NULL
      }

      yvar <- clean_data %>%
        dplyr::arrange("time") %>%
        dplyr::select(dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "y"), "ln.", ""), dep_var_basename))) %>%
        dplyr::pull()

      y.name <- paste0(ifelse(log_opts %in% c("both", "y"), "ln.", ""), dep_var_basename)

      xvars <- clean_data %>%
        dplyr::arrange("time") %>%
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
        dplyr::arrange("time") %>%
        dplyr::select(dplyr::all_of(paste0(ifelse(log_opts %in% c("both", "y"), "D.ln.", "D."), dep_var_basename))) %>%
        dplyr::pull()

      y.name <- paste0(ifelse(log_opts %in% c("both", "y"), "D.ln.", "D."), dep_var_basename)

      # TODO: Check log specification and check when model is AR only
      if(!identical(x_vars_basename, character(0))){
        xvars_names <- grep("L[0-9]\\.D.",
                            grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
                            value = TRUE
        )
      } else {
        xvars_names <- NULL
      }

      xvars <- clean_data %>%
        dplyr::arrange("time") %>%
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

    # Check for pre-specified models ------------------------------------------
    if(dep_var_basename %in% names(manual_models)){
      # check that manual_models[[dep_var_basename]] is either an isat or arx object
      # if not show a stop message explaining which manual model caused the error
      if(!inherits(manual_models[[dep_var_basename]], c("isat","arx"))){
        stop(paste0("The manual model for ",dep_var_basename," is not an 'isat' or 'arx' object."))
      }

      var_names_manual_mod <- manual_models[[dep_var_basename]]$aux$mXnames
      var_basenames_manual_mod <- gsub("^L[0-9]+\\.|ln\\.|D\\.","",grep("mconst|trend|ar[0-9]+|q_[0-9]+|^iis[0-9]+|^sis[0-9]+", var_names_manual_mod, value = TRUE, invert = TRUE))
      # check that all necessary variables are present in the specification for this module
      if(!all(var_basenames_manual_mod %in% x_vars_basename)){
        stop(paste0("The manual model for ",dep_var_basename," is missing the variable(s) in the specification for this module. Please add those to the specification, even if providing a manual model.\nMissing: ",var_basenames_manual_mod[!var_basenames_manual_mod %in% x_vars_basename],"\n"))
      }

      intermed.model <- manual_models[[dep_var_basename]]

      if(exists("intermed.model")){
        diagnostic_names <- names(intermed.model$diagnostics[,"p-value", drop = TRUE])
        diagnostic_pvalues <- intermed.model$diagnostics[,"p-value", drop = TRUE]
      }

      isat_list[i + 1, "AR"] <- if(exists("intermed.model")){diagnostic_pvalues[grepl("Ljung-Box AR\\([0-9]+\\)",diagnostic_names)]}else{NA}
      isat_list[i + 1, "ARCH"] <- if(exists("intermed.model")){diagnostic_pvalues[grepl("Ljung-Box ARCH\\([0-9]+\\)",diagnostic_names)]}else{NA}
      isat_list[i + 1, "BIC"] <- if(exists("intermed.model")){stats::BIC(intermed.model)}else{NA}
      isat_list[i + 1, "isat_object"] <- if(exists("intermed.model")){dplyr::tibble(isat_object = list(intermed.model))}else{NA}
    }


    # ISAT modelling ----------------------------------------------------------
    if (!dep_var_basename %in% names(manual_models) & !is.null(saturation) & !saturation_only_post_selection) {
      try(
        intermed.model <- run_isat(ar = if(i != 0){1:i} else {NULL},
                                   yvar = yvar,
                                   y.name = y.name,
                                   xvars  = xvars,
                                   mc = TRUE,
                                   clean_data = clean_data,
                                   saturation = saturation,
                                   saturation.tpval = saturation.tpval,
                                   max.block.size = max.block.size,
                                   pretest_steps = pretest_steps,
                                   determine.blocksize = TRUE)
        , silent = TRUE)
    } else if(!dep_var_basename %in% names(manual_models) ){

      # ARX Modelling -----------------------------------------------------------
      # Save original arx mc warning setting and disable it here
      tmpmc <- options("mc.warning")
      on.exit(options(tmpmc)) # set the old mc warning on exit

      options(mc.warning = FALSE)

      xvar_opts <- if(nrow(zoo::zoo(xvars, order.by = clean_data$time))>0){
        zoo::zoo(xvars, order.by = clean_data$time)
      } else {NULL}
      suppressWarnings(try(intermed.model <- gets::arx(
        y = zoo::zoo(yvar, order.by = clean_data$time),
        mxreg = xvar_opts,
        ar = if (i != 0) {
          1:i
        } else {
          NULL
        },
        singular.ok = TRUE,
        plot = FALSE
      ), silent = TRUE))

      if(exists("intermed.model")){
        colnames(intermed.model$aux$mX) <- intermed.model$aux$mXnames
        intermed.model$aux$args <- if(i != 0){list(ar = 1:i)} else {list(ar = NULL)}
        intermed.model$aux$y.name <- y.name
      }
    }

    if(exists("intermed.model")){
      diagnostic_names <- names(intermed.model$diagnostics[,"p-value", drop = TRUE])
      diagnostic_pvalues <- intermed.model$diagnostics[,"p-value", drop = TRUE]
    }

    isat_list[i + 1, "AR"] <- if(exists("intermed.model")){diagnostic_pvalues[grepl("Ljung-Box AR\\([0-9]+\\)",diagnostic_names)]}else{NA}
    isat_list[i + 1, "ARCH"] <- if(exists("intermed.model")){diagnostic_pvalues[grepl("Ljung-Box ARCH\\([0-9]+\\)",diagnostic_names)]}else{NA}
    isat_list[i + 1, "BIC"] <- if(exists("intermed.model")){stats::BIC(intermed.model)}else{NA}
    isat_list[i + 1, "isat_object"] <- if(exists("intermed.model")){dplyr::tibble(isat_object = list(intermed.model))}else{NA}
    if(exists("intermed.model")){rm(intermed.model)}
  }

  if (all(is.na(isat_list$BIC) | is.null(isat_list$BIC) | all(isat_list$isat_object %in% c(list(NA),list(NULL))))){
    dplyr::tibble(time = clean_data$time,
                  y = yvar,
                  xvars_initial) %>%
      dplyr::rename_with(.cols = "y",.fn = ~paste0(ifelse(log_opts %in% c("both", "y"), "D.ln.", "D."), dep_var_basename)) %>%
      dplyr::select(-c(dplyr::any_of(c("q_1", "q_2", "q_3", "q_4", "trend")))) %>%
      tidyr::pivot_longer(-c("time")) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$value, color = .data$name)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::facet_wrap(~.data$name, scales = "free_y", ncol = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = NULL, y = NULL) -> p
    print(p)

    stop(paste0("No model could be estimated for the module for ",dep_var_basename,
                ".\n Check the equation set-up and the data. Check also if there are missing variables that might lead to an empty sample.\n",
                "For debugging, a plot for this module has been produced - check if there are enough overlapping sample periods."))
  }

  # only choose between models where diagnostics are ok
  best_isat_model <- isat_list %>%
    dplyr::filter(.data$AR > 0.05 & .data$ARCH > 0.05) %>%
    {if(nrow(.) > 0){
      dplyr::filter(.,BIC == min(.data$BIC, na.rm = TRUE)) %>%
        dplyr::pull(dplyr::all_of("isat_object")) %>%
        dplyr::first()
    } else {NULL}}


  # if none remain, focus on AR
  if(is.null(best_isat_model)){
    best_isat_model <- isat_list %>%
      dplyr::filter(.data$AR > 0.05) %>%
      {if(nrow(.) > 0){
        dplyr::filter(.,BIC == min(.data$BIC, na.rm = TRUE)) %>%
          dplyr::pull(dplyr::all_of("isat_object")) %>%
          dplyr::first()
      } else {NULL}}
  }

  # if none remain, choose with BIC
  if(is.null(best_isat_model)){
    best_isat_model <- isat_list %>%
      dplyr::filter(BIC == min(.data$BIC, na.rm = TRUE)) %>%
      dplyr::pull(dplyr::all_of("isat_object")) %>%
      dplyr::first()
  }

  # gets selection on the best model ----------------------------------------
  if(gets_selection & !dep_var_basename %in% names(manual_models)){
    if(!is.null(keep)){keep_num <- which(grepl(keep, row.names(best_isat_model$mean.results)))} else {keep_num <- NULL}

    try(best_isat_model.selected <- gets::gets(best_isat_model,
                                               print.searchinfo = FALSE,
                                               t.pval = selection.tpval,
                                               ar.LjungB = NULL,
                                               arch.LjungB = NULL,
                                               keep = keep_num), silent = TRUE)

    if(!exists("best_isat_model.selected")){
      #if(!quiet){warning("Model selection with 'gets' failed. The best model is the one with the lowest BIC. Disable warning with 'quiet = TRUE'.")}
      best_isat_model.selected <- best_isat_model
    }

    colnames(best_isat_model.selected$aux$mX) <- best_isat_model.selected$aux$mXnames

    # make sure the ar values are retained correction
    ar_retained <- grep("^ar[0-9]+",best_isat_model.selected$aux$mXnames, value = TRUE)
    if(!identical(ar_retained, character(0))){
      best_isat_model.selected$aux$args$ar <- as.numeric(gsub("ar","",ar_retained))
    } else {
      best_isat_model.selected$aux$args <- list(ar = NULL)
    }
    ar_retained_num <- if(identical(ar_retained,character(0))){NULL}else{as.numeric(gsub("ar","",ar_retained))}

    retained.coefs <- row.names(best_isat_model.selected$mean.results)
    retained.coefs <- retained.coefs[!grepl("^mconst|^sis[0-9]+|^iis[0-9]+|^ar[0-9]+", retained.coefs)]
    retained.xvars <- as.matrix(xvars[,retained.coefs])

    retained.xvars <- if (!is.null(retained.xvars)){
      if(ncol(retained.xvars) > 0){
        zoo::zoo(retained.xvars, order.by = clean_data$time)
      }} else {NULL}

    if (!is.null(saturation) | saturation_only_post_selection) {
      best_isat_model.selected.isat <- run_isat(yvar = yvar,
                                                y.name = y.name,
                                                xvars  = retained.xvars,
                                                clean_data = clean_data,
                                                ar = ar_retained_num,
                                                mc = any(grepl("mconst",best_isat_model.selected$aux$mXnames)),
                                                saturation = saturation,
                                                saturation.tpval = saturation.tpval,
                                                determine.blocksize = FALSE,
                                                pretest_steps = pretest_steps,
                                                max.block.size = best_isat_model$aux$args$max.block.size)

      # best_isat_model.selected.isat <- gets::isat(y = zoo::zoo(yvar, order.by = clean_data$time),
      #                                             # ar = best_isat_model$aux$args$ar,
      #                                             # mc = best_isat_model$aux$args$mc,
      #                                             ar = ar_retained_num,
      #                                             mc = any(grepl("mconst",best_isat_model.selected$aux$mXnames)),
      #                                             mxreg = retained.xvars,
      #                                             plot = FALSE,
      #                                             print.searchinfo = FALSE,
      #                                             iis = ifelse("IIS" %in% saturation, TRUE, FALSE),
      #                                             sis = ifelse("SIS" %in% saturation, TRUE, FALSE),
      #                                             tis = ifelse("TIS" %in% saturation, TRUE, FALSE),
      #                                             t.pval = saturation.tpval,
      #                                             max.block.size = best_isat_model$aux$args$max.block.size,
      #                                             include.gum = FALSE)

      if(exists("best_isat_model.selected.isat")){best_isat_model.selected.isat$call$tis <- best_isat_model.selected.isat$aux$args$tis}
      if(exists("best_isat_model.selected.isat")){best_isat_model.selected.isat$aux$y.name <- y.name}
    }
  } else if(dep_var_basename %in% names(manual_models)){
    best_isat_model.selected <- manual_models[[dep_var_basename]]
    best_isat_model.selected.isat <- NULL
  }


  final_model <- if(gets_selection) {
    if (!is.null(saturation)) {
      best_isat_model.selected.isat
    } else {
      best_isat_model.selected
    }
  } else {
    best_isat_model
  }

  # Super Exogeneity Testing ------------------------------------------------
  suppressWarnings(try(superex_test <- super.exogeneity(final_model, saturation.tpval = saturation.tpval, quiet = quiet)))
  if(!exists("superex_test")){superex_test <- NA}

  # Output ------------------------------------------------------------------
  out <- list()
  out$isat_list <- isat_list
  #out$best_model <- isat_list %>%
  #  dplyr::filter(BIC == min(BIC)) %>%
  #  dplyr::pull(dplyr::all_of("isat_object")) %>%
  #  dplyr::first()
  out$best_model <- final_model
  out$superex_test <- superex_test

  out$args <- list(clean_data = clean_data,
                   dep_var_basename = dep_var_basename,
                   x_vars_basename = x_vars_basename,
                   use_logs = use_logs,
                   ardl_or_ecm = ardl_or_ecm,
                   max.ar = max.ar,
                   max.dl = max.dl)

  return(out)
}
