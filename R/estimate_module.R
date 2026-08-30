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
#' @param ecm_pretest Character. How to handle the ECM pretest when 'ardl_or_ecm' = 'ecm'. Must be one of
#' 'auto', 'diagnostic', or 'none'. Default is 'auto'. If 'auto', OSEM chooses
#' between the requested ECM, a fully differenced model, or a levels ARDL based
#' on unit-root and single-equation ECM diagnostics. If 'diagnostic', diagnostics
#' are stored but the requested ECM is still estimated. If 'none', the requested
#' ECM is estimated without pretesting.
#' @param ecm_unit_root_alpha Character. Significance level used by the unit-root
#' decision rule. Must be one of '1pct', '5pct', or '10pct'. Default is '5pct'.
#' @param ecm_coint_alpha Numeric. Significance level used for the single-equation
#' ECM level-block diagnostic. Default is 0.05.
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
#' @param indicator_compression Logical. Whether to compress the indicators selected by the
#' 'isat' function from the 'gets' package into a smaller number of indicators that still
#' capture the same outlier and structural break dynamics. Default is TRUE. Indicator compression
#' is only applied to the best model selected based on BIC and diagnostic tests,
#' not to all estimated models.
#' @param transformation_map A named character vector containing the
#' transformation applied to each model variable during data preparation.
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
                            ecm_pretest = "auto",
                            ecm_unit_root_alpha = "5pct",
                            ecm_coint_alpha = 0.05,
                            max.ar = 4,
                            max.dl = 2,
                            saturation = c("IIS", "SIS"),
                            saturation.tpval = 0.01,
                            max.block.size = 20,
                            gets_selection = TRUE,
                            selection.tpval = 0.01,
                            keep,
                            pretest_steps,
                            indicator_compression = TRUE,
                            quiet = FALSE,
                            module,
                            transformation_map) {
  # Set-up ------------------------------------------------------------------
  log_opts <- use_logs
  level_x_vars_basename <- x_vars_basename

  if (!ardl_or_ecm %in% c("ardl", "ecm")) {
    stop("The variable 'ardl_or_ecm' in the 'estimate_module()' or the 'run_model()' function must be either 'ecm' or 'ardl'. You have supplied a different value.")
  }

  if (!ecm_pretest %in% c("auto", "diagnostic", "none")) {
    stop("The variable 'ecm_pretest' in the 'estimate_module()' or the 'run_model()' function must be either 'auto', 'diagnostic', or 'none'. You have supplied a different value.")
  }

  if (!ecm_unit_root_alpha %in% c("1pct", "5pct", "10pct")) {
    stop("The variable 'ecm_unit_root_alpha' in the 'estimate_module()' or the 'run_model()' function must be either '1pct', '5pct', or '10pct'. You have supplied a different value.")
  }

  model_form <- ardl_or_ecm

  ecm_decision <- list(
    requested = ardl_or_ecm,
    pretest = ecm_pretest,
    selected = ardl_or_ecm,
    model_form = ardl_or_ecm,
    reason = "ECM pretesting was not requested because this module was not estimated as an ECM.",
    integration = NULL,
    coint_test = NULL
  )

  # ECM pretest --------------------------------------------------------------
  if (ardl_or_ecm == "ecm") {
    ecm_decision$reason <- "ECM pretesting was disabled; estimating the requested unrestricted ECM."

    if (ecm_pretest != "none") {
      integration <- classify_module_integration(
        clean_data = clean_data,
        dep_var_basename = dep_var_basename,
        x_vars_basename = x_vars_basename,
        use_logs = use_logs,
        max.ar = max.ar,
        alpha = ecm_unit_root_alpha,
        selectlags = "BIC"
      )

      integration_for_decision <- integration

      # Adjust integration orders for the deterministic specification actually used
      # A trend-stationary variable is only treated as I(0) if the equation is
      # allowed to contain a deterministic trend. If trend = FALSE, then ECM-auto
      # treats trend-stationary variables conservatively as "uncertain".
      if (!trend) {
        integration_for_decision <- integration_for_decision %>%
          dplyr::mutate(
            order = dplyr::if_else(
              .data$stationarity_type %in% "trend_stationary",
              "uncertain",
              .data$order
            ),
            reason = dplyr::if_else(
              .data$stationarity_type %in% "trend_stationary",
              paste0(
                .data$reason,
                " Trend-stationary variable treated as uncertain because trend = FALSE."
              ),
              .data$reason
            )
          )
      }

      ecm_decision$integration <- integration
      ecm_decision$integration_for_decision <- integration_for_decision

      level_x_vars_basename <- integration_for_decision %>%
        dplyr::filter(.data$type == "independent", .data$order == "I1") %>%
        dplyr::pull(.data$basevarname)

      dep_order <- integration_for_decision %>%
        dplyr::filter(.data$type == "dependent") %>%
        dplyr::pull("order") %>%
        dplyr::first()

      x_orders <- integration_for_decision %>%
        dplyr::filter(.data$type == "independent") %>%
        dplyr::pull("order")

      if (ecm_pretest == "diagnostic") {
        coint_test <- test_single_equation_ecm(
          clean_data = clean_data,
          dep_var_basename = dep_var_basename,
          x_vars_basename = x_vars_basename,
          level_x_vars_basename = level_x_vars_basename,
          use_logs = use_logs,
          trend = trend,
          module = module,
          alpha = ecm_coint_alpha,
          transformation_map = transformation_map
        )

        ecm_decision$coint_test <- coint_test
        ecm_decision$selected <- "ecm"
        ecm_decision$model_form <- "ecm"
        ecm_decision$reason <- "Diagnostic ECM pretesting was requested; estimating the requested ECM regardless of the diagnostic decision."
      }

      if (ecm_pretest == "auto") {
        if (all(integration_for_decision$order == "I0", na.rm = TRUE)) {
          model_form <- "ardl"

          ecm_decision$selected <- "ardl"
          ecm_decision$model_form <- "ardl"
          ecm_decision$reason <- "All module variables were classified as I(0); estimating a levels ARDL."
        } else if (any(integration_for_decision$order %in% c("I2_or_uncertain", "uncertain"), na.rm = TRUE)) {
          model_form <- "diff"

          ecm_decision$selected <- "fully_differenced"
          ecm_decision$model_form <- "diff"

          if(!trend & any(integration_for_decision$stationarity_type == "trend_stationary", na.rm = TRUE)){
            ecm_decision$reason <- "At least one module variable was classified as I(2) or uncertain; estimating the corresponding first-differenced equation. Note that at least one variable was classified as trend-stationary and treated as uncertain because trend = FALSE."
          } else {
            ecm_decision$reason <- "At least one module variable was classified as I(2) or uncertain; estimating the corresponding first-differenced equation."
          }


        } else if (!identical(dep_order, "I1")) {
          model_form <- "diff"

          ecm_decision$selected <- "fully_differenced"
          ecm_decision$model_form <- "diff"
          ecm_decision$reason <- "The dependent variable was not classified as I(1); estimating the corresponding first-differenced equation."
        } else if (!any(x_orders == "I1", na.rm = TRUE)) {
          model_form <- "diff"

          ecm_decision$selected <- "fully_differenced"
          ecm_decision$model_form <- "diff"
          ecm_decision$reason <- "No conditioning variable was classified as I(1); estimating the corresponding first-differenced equation."
        } else {
          coint_test <- test_single_equation_ecm(
            clean_data = clean_data,
            dep_var_basename = dep_var_basename,
            x_vars_basename = x_vars_basename,
            level_x_vars_basename = level_x_vars_basename,
            use_logs = use_logs,
            trend = trend,
            module = module,
            alpha = ecm_coint_alpha,
            transformation_map = transformation_map
          )

          ecm_decision$coint_test <- coint_test

          if (isTRUE(coint_test$decision)) {
            model_form <- "ecm"

            ecm_decision$selected <- "ecm"
            ecm_decision$model_form <- "ecm"
            ecm_decision$reason <- coint_test$reason
          } else {
            model_form <- "diff"

            ecm_decision$selected <- "fully_differenced"
            ecm_decision$model_form <- "diff"
            ecm_decision$reason <- coint_test$reason
          }
        }
      }
    }
  }

  # ECM keep terms -----------------------------------------------------------
  # If the final model form is ECM, protect the lagged level block during the
  # subsequent gets() selection step. Otherwise, the automatic model selection
  # could remove the terms that give the equation its equilibrium-correction
  # interpretation.
  ecm_keep <- NULL

  if (model_form == "ecm") {
    if (!is.null(ecm_decision$coint_test) && length(ecm_decision$coint_test$level_terms) > 0) {
      ecm_keep <- ecm_decision$coint_test$level_terms
    } else {
      ecm_keep <- c(
        paste0(ifelse(log_opts %in% c("both", "y"), "L1.ln.", "L1."), dep_var_basename),
        if (!identical(x_vars_basename, character(0))) {
          paste0(ifelse(log_opts %in% c("both", "x"), "L1.ln.", "L1."), x_vars_basename)
        } else {
          NULL
        }
      )
    }
  }

  isat_list <- dplyr::tibble(
    ar = 0:max.ar,
    BIC = NA,
    ar_pvalue = NA,
    arch_pvalue = NA,
    isat_object = list(NA_complex_)
  )
  design_term_specs <- list()

  for (i in 0:max.dl) {
    # Build model design -----------------------------------------------------
    # The logic for ARDL, ECM, differenced models, and lag-only variables lives
    # in build_module_design(). This keeps clean_data complete for diagnostics
    # while restricting only the estimation design matrix.
    design <- build_module_design(
      clean_data = clean_data,
      dep_var_basename = dep_var_basename,
      x_vars_basename = x_vars_basename,
      use_logs = use_logs,
      trend = trend,
      model_form = model_form,
      dl_order = i,
      module = module,
      transformation_map = transformation_map
    )

    yvar <- design$yvar
    y.name <- design$y.name
    xvars <- design$xvars
    design_term_specs[[i + 1L]] <- design$term_spec

    if (i == 0) {
      xvars_initial <- xvars
    }

    # ISAT modelling ----------------------------------------------------------
    if (!is.null(saturation)) {
      try(
        intermed.model <- run_isat(
          ar = if(i != 0){1:i} else {NULL},
          yvar = yvar,
          y.name = y.name,
          xvars  = xvars,
          mc = TRUE,
          clean_data = clean_data,
          saturation = saturation,
          saturation.tpval = saturation.tpval,
          max.block.size = max.block.size,
          pretest_steps = pretest_steps,
          determine.blocksize = TRUE
        ),
        silent = TRUE
      )
    } else {

      # ARX Modelling ---------------------------------------------------------
      # Save original arx mc warning setting and disable it here
      tmpmc <- options("mc.warning")
      on.exit(options(tmpmc)) # set the old mc warning on exit

      options(mc.warning = FALSE)

      xvar_opts <- if(nrow(zoo::zoo(xvars, order.by = clean_data$time)) > 0){
        zoo::zoo(xvars, order.by = clean_data$time)
      } else {
        NULL
      }

      intermed.model <- gets::arx(
        y = zoo::zoo(yvar, order.by = clean_data$time),
        mxreg = xvar_opts,
        ar = if (i != 0) {
          1:i
        } else {
          NULL
        },
        plot = FALSE
      )

      colnames(intermed.model$aux$mX) <- intermed.model$aux$mXnames
      intermed.model$aux$args <- if(i != 0){list(ar = 1:i)} else {list(ar = NULL)}
      intermed.model$aux$y.name <- y.name
    }
    if(exists("intermed.model")){
      diagnostics <- intermed.model$diagnostics %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(diagnostic = row.names(intermed.model$diagnostics), .before = "Chi-sq")
    }

    isat_list[i + 1, "ar_pvalue"] <- if(exists("intermed.model")){diagnostics$`p-value`[grep("Ljung-Box AR\\(",diagnostics)]}else{NA}
    isat_list[i + 1, "arch_pvalue"] <- if(exists("intermed.model")){diagnostics$`p-value`[grep("Ljung-Box ARCH\\(",diagnostics)]}else{NA}
    isat_list[i + 1, "BIC"] <- if(exists("intermed.model")){stats::BIC(intermed.model)}else{NA}
    isat_list[i + 1, "isat_object"] <- if(exists("intermed.model")){dplyr::tibble(isat_object = list(intermed.model))}else{NA}

    if(exists("intermed.model")){
      rm(intermed.model)
    }
  }

  if (all(is.na(isat_list$BIC) | is.null(isat_list$BIC) | all(isat_list$isat_object %in% c(list(NA),list(NULL))))){
    dplyr::tibble(time = clean_data$time,
                  y = yvar,
                  xvars_initial) %>%
      dplyr::rename_with(.cols = "y",.fn = ~y.name) %>%
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

  best_isat_model <- isat_list %>%
    dplyr::mutate(diag_ranking = dplyr::case_when((.data$ar_pvalue > 0.05) & (.data$arch_pvalue > 0.05) ~ 1,
                                                  (.data$ar_pvalue > 0.05) ~ 2,
                                                  (.data$arch_pvalue > 0.05) ~ 3,
                                                  TRUE ~ 4)) %>%
    dplyr::filter(diag_ranking == min(diag_ranking, na.rm = TRUE)) %>%
    dplyr::filter(BIC == min(dplyr::pick("BIC"), na.rm = TRUE)) %>%
    dplyr::pull(dplyr::all_of("isat_object")) %>%
    dplyr::first()

  # gets selection on the best model ----------------------------------------
  if(gets_selection){

    #keep <- paste0("^mc$|^ar[0-9]+$|^q_[0-9]+|",keep)

    # Keep handling ----------------------------------------------------------
    if(!is.null(keep)){
      keep_user_num <- which(grepl(keep, row.names(best_isat_model$mean.results)))
    } else {
      keep_user_num <- integer(0)
    }

    if(!is.null(ecm_keep)){
      keep_ecm_num <- which(row.names(best_isat_model$mean.results) %in% ecm_keep)
    } else {
      keep_ecm_num <- integer(0)
    }

    keep_num <- unique(c(keep_user_num, keep_ecm_num))
    if(length(keep_num) == 0){
      keep_num <- NULL
    }
    if( "isat" %in% class(best_isat_model)){
      best_isat_model.arx <- as.arx.isat.osem(best_isat_model)
    } else {
      best_isat_model.arx <- best_isat_model
    }
    try(best_isat_model.selected <- gets::gets(best_isat_model.arx,
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
    retained.coefs <- retained.coefs[!grepl("^mconst|^sis[0-9]+|^iis[0-9]+|^tis[0-9]+|^ar[0-9]+", retained.coefs)]
    retained.xvars <- as.matrix(xvars[,retained.coefs])

    retained.xvars <- if (!is.null(retained.xvars)){
      if(ncol(retained.xvars) > 0){
        zoo::zoo(retained.xvars, order.by = clean_data$time)
      }} else {NULL}

    if (!is.null(saturation)) {
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

      if(exists("best_isat_model.selected.isat")){
        best_isat_model.selected.isat$call$tis <- best_isat_model.selected.isat$aux$args$tis
      }
      if(exists("best_isat_model.selected.isat")){
        best_isat_model.selected.isat$aux$y.name <- y.name
      }
    }
  }

  model_before_compression <- if(gets_selection) {
    if (!is.null(saturation)) {
      best_isat_model.selected.isat
    } else {
      best_isat_model.selected
    }
  } else {
    best_isat_model
  }

  if(indicator_compression){
    compression <- compress_indicators(model_before_compression)
    final_model <- compression$compressed_model
  } else {
    final_model <- model_before_compression
  }


  # Super Exogeneity Testing ------------------------------------------------
  try(superex_test <- super.exogeneity(final_model, saturation.tpval = saturation.tpval, quiet = quiet))
  if(!exists("superex_test")){
    superex_test <- NA
  }

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
                   transformations = transformation_map,
                   forecast_recipe = compile_forecast_recipe(
                     model_object = final_model,
                     model_form = model_form,
                     dep_var_basename = dep_var_basename,
                     x_vars_basename = x_vars_basename,
                     use_logs = use_logs,
                     transformations = transformation_map,
                     term_specs = dplyr::bind_rows(design_term_specs),
                     lag_only_vars = design$lag_only_vars
                   ),
                   # Retained for backwards compatibility. New forecasting code
                   # uses the unambiguous model_form field below.
                   ardl_or_ecm = ifelse(model_form == "diff", "ecm", model_form),
                   ardl_or_ecm_requested = ardl_or_ecm,
                   ardl_or_ecm_selected = ecm_decision$selected,
                   model_form = model_form,
                   ecm_pretest = ecm_pretest,
                   ecm_decision = ecm_decision,
                   ecm_keep = ecm_keep,
                   max.ar = max.ar,
                   max.dl = max.dl)

  return(out)
}
