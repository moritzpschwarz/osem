
forecasting_table_ijf <- function(forecast, selected_vars, accuracy = 2){

  fc_ar$full_forecast_data %>%
    filter(grepl(selected_vars, na_item),
           #!type %in% c("Insample Fit", "Observation", "Nowcast")) %>%
           type == "Forecast") %>%
    arrange(na_item, time) %>%
    filter(!values == p95) %>%
    mutate(across(c(values, starts_with("p")), ~round(., digits = accuracy)),
           Range = paste0(p025, " - ", p975)) %>%
    select(-starts_with("p"),
           -type) %>%

    relocate(na_item) -> forecast_df

  fc_metric <- tibble(`Forecast Metric` = c(
    "RMSFE with AR Baseline:",
    "",
    "MAPE with AR Baseline:",
    "",
    "RMSFE with RW Baseline:",
    "",
    "MAPE with RW Baseline:",
    "",
    "Mincer-Zarnowitz:",
    ""
  )) %>%
    slice(rep(1:10, length(unique(forecast_df$na_item))))

  forecast_df %>%
    bind_cols(fc_metric) %>%
    mutate(`Actual Value` = NA, .after = Range) %>%
    select(-na_item) %>%
    kable("latex", escape = TRUE, align = "c", col.names = c("Forecast Period", "Central", "95% Range","Actual Value", "Forecast Metrics"), booktabs = TRUE) %>%
    kable_styling(font_size = 7.5) %>%
    #column_spec(4, width = "5cm") %>%
    pack_rows(index = table(forecast_df$na_item))
}



table_change <- function(input_table, label){

  lab_use <- if(!is.null(label)){paste0("\\label{",label,"}")} else {""}

  # insert caption
  tab <- gsub("\\begin{table}",
              paste0("\\begin{table}\n",lab_use), input_table, fixed = TRUE)

  # insert resizebox
  tab <- gsub("\\begin{tabular}",
              "\\resizebox{\\textwidth}{!}{\\begin{tabular}", tab, fixed = TRUE)

  # fixing the footer
  tab <- gsub("\\end{tabular}",
              "\\end{tabular}}",tab, fixed = TRUE)

  return(tab)
}



extract_central_forecasts <- function(forecast){

  if(!is.null(forecast$orig_model$opts_df[["log_opts"]])){
    forecast$orig_model$opts_df %>%
      dplyr::mutate(log_opts_dependent = purrr::map2(.data$log_opts, .data$dependent, function(opts,dep){
        opts[,dep, drop = TRUE]
      })) %>%
      tidyr::unnest("log_opts_dependent", keep_empty = TRUE) %>%
      tidyr::replace_na(list(log_opts_dependent = "none")) %>%
      dplyr::select(c("dep_var" = "dependent","log_opt" = "log_opts_dependent")) -> log_opts_processed
  } else {
    log_opts_processed <- dplyr::tibble(dep_var = forecast$orig_model$opts_df$dependent, log_opt = "none")
  }

  forecast$forecast %>%
    dplyr::select("dep_var", "central.estimate") %>%
    tidyr::unnest("central.estimate") %>%
    tidyr::pivot_longer(-c("time","dep_var")) %>%
    tidyr::drop_na() %>%
    dplyr::full_join(log_opts_processed, by = "dep_var") %>%
    dplyr::mutate(value = dplyr::case_when(.data$log_opt == "log" ~ exp(.data$value),
                                           .data$log_opt == "asinh" ~ sinh(.data$value),
                                           .data$log_opt == "none" ~ .data$value)) %>%
    dplyr::select(-c("name", "log_opt")) %>%
    dplyr::rename(forecast = "value",
                  na_item = "dep_var") -> forecasts_processed

  return(forecasts_processed)

}


trafo_fun <- function(x) {
  stopifnot(length(x) == 1L)
  if (grepl("ln\\.", x)) {
    var <- sub(".*\\.", "", x)
    tr <- trafo %>% filter(variable == var) %>% pull(trafo)
    xtrans <- paste0(str_replace(x, "ln\\.", paste0(tr, "(")), ")")
    return(xtrans)
  } else {
    return(x)
  }
}

