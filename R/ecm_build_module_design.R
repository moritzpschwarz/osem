build_module_design <- function(clean_data,
                                dep_var_basename,
                                x_vars_basename,
                                use_logs = "both",
                                trend = TRUE,
                                model_form = c("ardl", "ecm", "diff"),
                                dl_order = 0,
                                module) {

  parse_lag_only_vars <- function(module) {
    if (is.null(module$lag) || is.na(module$lag) || identical(module$lag, "")) {
      return(character(0))
    }

    lag_only_vars <- trimws(unlist(strsplit(module$lag, ",")))
    lag_only_vars <- lag_only_vars[!is.na(lag_only_vars)]
    lag_only_vars <- lag_only_vars[lag_only_vars != ""]

    return(lag_only_vars)
  }


    model_form <- match.arg(model_form)
    log_opts <- use_logs

    lag_only_vars <- parse_lag_only_vars(module)
    x_vars_contemp <- setdiff(x_vars_basename, lag_only_vars)

    if (model_form == "ardl") {
      if (!identical(x_vars_basename, character(0))) {
        xvars_names <- grep(
          "^L[0-9]\\.",
          grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
          value = TRUE
        )

        xvars_names <- xvars_names[!grepl("^L[0-9]\\.D\\.", xvars_names)]

        if (log_opts %in% c("y", "none")) {
          xvars_names <- xvars_names[!grepl("ln\\.", xvars_names)]
        } else {
          xvars_names <- xvars_names[grepl("ln\\.", xvars_names)]
        }
      } else {
        xvars_names <- NULL
      }

      y.name <- paste0(
        ifelse(log_opts %in% c("both", "y"), "ln.", ""),
        dep_var_basename
      )

      yvar <- clean_data %>%
        dplyr::select(dplyr::all_of(y.name)) %>%
        dplyr::pull()

      xvars <- clean_data %>%
        dplyr::select(
          if (trend) {
            dplyr::all_of("trend")
          } else {
            NULL
          },
          if (!identical(x_vars_contemp, character(0))) {
            dplyr::all_of(
              paste0(
                ifelse(log_opts %in% c("both", "x"), "ln.", ""),
                x_vars_contemp
              )
            )
          } else {
            NULL
          },
          if (dl_order != 0) {
            dplyr::all_of(
              xvars_names[
                grepl(paste0("^L", 1:dl_order, collapse = "|"), xvars_names)
              ]
            )
          } else {
            NULL
          },
          dplyr::any_of(c("q_2", "q_3", "q_4"))
        )
    }

    if (model_form == "ecm") {
      y.name <- paste0(
        ifelse(log_opts %in% c("both", "y"), "D.ln.", "D."),
        dep_var_basename
      )

      yvar <- clean_data %>%
        dplyr::select(dplyr::all_of(y.name)) %>%
        dplyr::pull()

      if (!identical(x_vars_basename, character(0))) {
        xvars_names <- grep(
          "L[0-9]\\.D.",
          grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
          value = TRUE
        )
      } else {
        xvars_names <- NULL
      }

      xvars <- clean_data %>%
        dplyr::select(
          dplyr::all_of(
            paste0(
              ifelse(log_opts %in% c("both", "y"), "L1.ln.", "L1."),
              dep_var_basename
            )
          ),
          if (!identical(x_vars_basename, character(0))) {
            dplyr::all_of(
              paste0(
                ifelse(log_opts %in% c("both", "x"), "L1.ln.", "L1."),
                x_vars_basename
              )
            )
          } else {
            NULL
          },
          if (!identical(x_vars_contemp, character(0))) {
            dplyr::all_of(
              paste0(
                ifelse(log_opts %in% c("both", "x"), "D.ln.", "D."),
                x_vars_contemp
              )
            )
          } else {
            NULL
          },
          if (dl_order != 0) {
            dplyr::all_of(
              xvars_names[
                grepl(paste0("^L", 1:dl_order, collapse = "|"), xvars_names)
              ]
            )
          } else {
            NULL
          },
          dplyr::any_of(c("q_2", "q_3", "q_4"))
        )
    }

    if (model_form == "diff") {
      y.name <- paste0(
        ifelse(log_opts %in% c("both", "y"), "D.ln.", "D."),
        dep_var_basename
      )

      yvar <- clean_data %>%
        dplyr::select(dplyr::all_of(y.name)) %>%
        dplyr::pull()

      if (!identical(x_vars_basename, character(0))) {
        xvars_names <- grep(
          "L[0-9]\\.D.",
          grep(paste0(x_vars_basename, collapse = "|"), names(clean_data), value = TRUE),
          value = TRUE
        )
      } else {
        xvars_names <- NULL
      }

      xvars <- clean_data %>%
        dplyr::select(
          if (!identical(x_vars_contemp, character(0))) {
            dplyr::all_of(
              paste0(
                ifelse(log_opts %in% c("both", "x"), "D.ln.", "D."),
                x_vars_contemp
              )
            )
          } else {
            NULL
          },
          if (dl_order != 0) {
            dplyr::all_of(
              xvars_names[
                grepl(paste0("^L", 1:dl_order, collapse = "|"), xvars_names)
              ]
            )
          } else {
            NULL
          },
          dplyr::any_of(c("q_2", "q_3", "q_4"))
        )
    }

    return(list(
      yvar = yvar,
      y.name = y.name,
      xvars = xvars,
      lag_only_vars = lag_only_vars,
      x_vars_contemp = x_vars_contemp
    ))
  }
