
forecasting_table_ijf <- function(forecast, selected_vars, accuracy = 2, label = "", caption = ""){

  forecast$full_forecast_data %>%
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
    kable("latex", escape = TRUE, align = "c",
          col.names = c("Forecast Period", "Central", "95% Range","Actual Value", "Forecast Metrics"),
          booktabs = TRUE, label = label,
          caption = caption) %>%
    kable_styling(font_size = 7) %>%
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





create_inflation <- function(model){

  # create inflation from HICP module
  hicp <- model$full_data %>%
    filter(na_item == "HICPlocal.hat") %>%
    select(time, values) %>%
    rename(HICP.hat = values) %>%
    arrange(time) %>%
    mutate(values = (HICP.hat - lag(HICP.hat, 1)) / lag(HICP.hat, 1) * 100) %>%
    mutate(na_item = "Inflation.hat") %>%
    select(time, na_item, values)
  inf <- model$full_data %>%
    filter(na_item == "HICPlocal") %>%
    select(time, values) %>%
    rename(HICP = values) %>%
    arrange(time) %>%
    mutate(values = (HICP - lag(HICP, 1)) / lag(HICP, 1) * 100) %>%
    mutate(na_item = "Inflation") %>%
    select(time, na_item, values)
  model$full_data <- bind_rows(model$full_data, hicp, inf)
  return(model)
}



create_regression_table_ijf <- function(model, grepl_selected = NULL, country = "DE", label_add = "", no_models_in_one_table = 5, caption_add = ""){
  # To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\num{}`, call:
  #   options("modelsummary_format_numeric_latex" = "plain")

  # remove NULL results
  model_list <- model$module_collection$model[!sapply(model$module_collection$model, is.null)]

  model_list <- lapply(model_list, function(x){if(!is.null(x)){gets::as.lm(x)}})

  names(model_list) <- model$module_order$dependent[!sapply(model$module_collection$model, is.null)]

  # ensure the correct order of the table
  lapply(model_list, broom::tidy) %>%
    bind_rows() %>%
    distinct(term) %>%
    filter(!str_detect(term, "iis|sis|q_[0-9]+")) %>%
    mutate(base = gsub("L[0-9]+\\.","",term)) %>%
    mutate(base_num = case_when(base == "mconst" ~ 2,
                                base == "trend" ~ 3,
                                grepl("ar[0-9]+",base) ~ 1,
                                TRUE ~ 4)) %>%
    arrange(base_num, base, desc(term)) %>%
    pull(term) -> coef_order

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


  # extract transformations
  # technically, the transformation of the same var could differ across modules because of different samples
  # need to ensure first the transformations are the same (otherwise cannot be same row in output latex table)
  trafo <- bind_rows(model$opts_df %>% pull(log_opts))
  trafo <- trafo %>%
    reframe(across(everything(), ~ na.omit(unique(.x))))
  stopifnot(NROW(trafo) == 1L) # then can use same transformation in all tables
  trafo <- trafo %>% pivot_longer(everything(), names_to = "variable", values_to = "trafo")

  coef_renamed <- sapply(coef_order, trafo_fun)
  names(coef_renamed) <- coef_order # names should already be set but to be sure


  if(is.null(grepl_selected)){

    no_of_tables <- ceiling(length(model_list)/no_models_in_one_table)

    for(i in 1:no_of_tables){
      # i = 1
      model_list[cut(1:length(model_list), breaks = no_of_tables, labels = FALSE) == i] -> model_list_temp
      models_renamed <- sapply(names(model_list_temp), function(x) paste0(trafo %>% filter(variable == x) %>% pull(trafo), "(", x, ")"))
      names(model_list_temp) <- models_renamed

      table_output <- modelsummary::modelsummary(
        model_list_temp,
        #coef_omit = "iis|sis|q_[0-9]+",
        coef_map = coef_renamed,
        gof_omit = "R",
        output = "latex",
        title = paste0("Final OSEM Model result for each module for ",country_long, caption_add, ". Part ",i,"."),
        notes = "Quarterly Dummies, Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
        stars = TRUE
      )%>%
        kable_styling(font_size = 8)

      # this output can go straight into the latex
      table_change(table_output, label = paste0("tab:regression_summary",country,"_",i, label_add)) %>%
        writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Regression_Summary_",i,label_add,".tex"))

    }
  }
  if(!is.null(grepl_selected)){

    ## selected variables ------------------------------------------------------

    model_list[grepl(gsub("Real","",vars_to_grab), names(model_list))] -> model_list_temp
    models_renamed <- sapply(names(model_list_temp), function(x) paste0(trafo %>% filter(variable == x) %>% pull(trafo), "(", x, ")"))
    names(model_list_temp) <- models_renamed

    table_output <- modelsummary::modelsummary(
      model_list_temp,
      #coef_omit = "iis|sis|q_[0-9]+",
      coef_map = coef_renamed,
      gof_omit = "R",
      output = "latex",
      title = paste0("Final OSEM Model result for selected modules for ",country_long, caption_add, "."),
      notes = "Quarterly Dummies, Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
      stars = TRUE
    ) %>%
      kable_styling(font_size = 8)

    # this output can go straight into the latex
    table_change(table_output, label = paste0("tab:regression_summary_selected",country, label_add)) %>%
      writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Regression_Summary_selected",label_add,".tex"))

  }
}
