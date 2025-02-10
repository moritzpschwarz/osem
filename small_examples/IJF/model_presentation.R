library(tidyverse)
library(kableExtra)
library(ggtext)
library(modelsummary)
devtools::load_all()

vars_to_grab <- "ElectrCons|EmiCO2RoaTra|EmiCO2ManInd|EmiCO2ElecHeat|RealVAIndustry|RealConsHH"
vars_for_spec_table <- paste0(vars_to_grab, "|VAIndustry|HICP|CapForm|CapFormHH")
no_modles_in_one_table <- 5

# define the main country to leave out of the appendix
main_country <- "DE"
all_countries <- c("DE","AT","FR","DK")

# control what to run right now
run_modelplots <- TRUE
plot_forecasts <- TRUE
run_insample <- FALSE
run_inventory_diagnostics <- TRUE
run_network <- TRUE
run_scenario <- FALSE
run_fc_comparison <- FALSE

lwidth <- 0.75

source("small_examples/IJF/model_presentation_functions.R")


for(country in all_countries){

  print(country)
  # country = "DE"

  # Loading the model -------------------------------------------------------

  load(paste0("./small_examples/IJF/", country, "/model_sel.RData"))

  model_result_ext <- model_result_ext_sel

  country_long <- case_when(country == "DE" ~ "Germany",
                            country == "DK" ~ "Denmark",
                            country == "AT" ~ "Austria",
                            country == "FR" ~ "France")

  # create inflation from HICP module
  hicp <- model_result_ext$full_data %>%
    filter(na_item == "HICPlocal.hat") %>%
    select(time, values) %>%
    rename(HICP.hat = values) %>%
    arrange(time) %>%
    mutate(values = (HICP.hat - lag(HICP.hat, 1)) / lag(HICP.hat, 1) * 100) %>%
    mutate(na_item = "Inflation2.hat") %>%
    select(time, na_item, values)
  inf2 <- model_result_ext$full_data %>%
    filter(na_item == "Inflation") %>%
    mutate(na_item = "Inflation2") %>%
    arrange(time)
  model_result_ext$full_data <- bind_rows(model_result_ext$full_data, hicp, inf2)
  model_result_comp <- model_result_ext
  # create plot for comparison as sanity check
  plot(model_result_comp, grepl_variables = "Inflation", title = paste0("OSEM Model Output for ",country_long), linewidth = 0.75)
  # now that have compared, replaced the original inflation with the HICP one
  model_result_ext$full_data <- model_result_ext$full_data %>%
    filter(na_item != "Inflation") %>%
    filter(na_item != "Inflation.hat") %>%
    mutate(na_item = case_when(na_item == "Inflation2" ~ "Inflation",
                               na_item == "Inflation2.hat" ~ "Inflation.hat",
                               TRUE ~ na_item))

  # Specification Table -----------------------------------------------------

  if(!file.exists(paste0("small_examples/IJF/tables_overleaf/general/Specification_all_countries.tex"))){
    model_result_ext$args$specification %>%
      mutate(type = case_when(type == "n" ~ "Estimated",
                              type == "d" ~ "Identity",
                              TRUE ~ "Exogenous"),
             Model = 1:n(),
             independent = gsub("Factor","100",independent)) %>%
      mutate(independent = case_when(type == "Estimated" ~ paste0("f(",gsub(" \\+ ",", ",independent), ")"),
                                     TRUE ~ independent)) %>%

      relocate(Model) %>%
      rename_with(str_to_title) -> full_table


    full_table %>%
      kable(format = "latex", booktabs = TRUE, label = "spec_full",
            caption = "Specification of individual modules and their linkages. All equations are specified to potentially also include autoregressive lags. Functions specified as f() are AR Models.") %>%
      kable_styling(font_size = 8) %>%
      column_spec(4, width = "10cm") %>%
      writeLines(paste0("small_examples/IJF/tables_overleaf/general/Specification_all_countries.tex"))


    full_table %>%
      filter(grepl(vars_for_spec_table, Dependent)) %>%


      kable(format = "latex", booktabs = TRUE, label = "spec_subset",
            caption = "Selected specification of individual modules and their linkages. All equations are specified to potentially also include autoregressive lags. Functions specified as f() are AR Models. The full model specification table is available in the Appendix.") %>%
      kable_styling() %>%
      kable_styling(font_size = 8) %>%
      column_spec(4, width = "10cm") %>%
      #table_change(label = "spec_subset") %>%
      writeLines(paste0("small_examples/IJF/tables_overleaf/general/Specification_selected.tex"))
  }





  # Model plots -------------------------------------------------------------
  # colors <- c(
  #   "Forecast/Assumption of\nExogenous Variables" = "#31688EFF",
  #   "Forecast" = "#3B528BFF",
  #   "Insample Fit" = "#FDE725FF",
  #   "Observation" = "#440154FF",
  #   "Nowcast" = "#35B779FF"
  # )

  mod_subtitle <- "Showing the <span style = color:#440154FF>Observed</span> and <span style = color:#FDE725FF>Fitted</span> values."
  fc_subtitle <- "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span>, <span style = color:#35B779FF>Nowcasted</span>, and <span style = color:#3B528BFF>Forecasted</span> values."

  ## Huge Model plot ---------------------------------------------------------
  if(run_modelplots){

    plot(model_result_ext, title = paste0("OSEM Model Output for ",country_long), linewidth = lwidth) +
      labs(subtitle = mod_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Model",".pdf"), width = 12, height = 10)

    ## Model plot with selected variables ---------------------------------------------------------
    plot(model_result_ext, grepl_variables = vars_to_grab, title = paste0("OSEM Model Output for ",country_long), linewidth = lwidth) +
      labs(subtitle = mod_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Model_Selected",".pdf"), width = 7, height = 5)


    # Same Model Plot from 2015 -----------------------------------------------
    plot(model_result_ext, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("OSEM Model Output for ",country_long), linewidth = lwidth) +
      labs(subtitle = mod_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Model_Selected_2015",".pdf"), width = 7, height = 5)
  }
  # Forecast ----------------------------------------------------------------
  # auto
  if(!file.exists(paste0("small_examples/IJF/", country, "/forecast.RData"))){
    set.seed(8899)
    fc <- forecast_model(model_result_ext, exog_fill_method = "auto")
    save(fc, file = paste0("small_examples/IJF/", country, "/forecast.RData"))
  } else {
    load(paste0("small_examples/IJF/", country, "/forecast.RData"))
  }

  # ets
  if(!file.exists(paste0("small_examples/IJF/", country, "/forecast_ets.RData"))){
    set.seed(8899)
    fc_ets <- forecast_model(model_result_ext, exog_fill_method = "ets")
    save(fc_ets, file = paste0("small_examples/IJF/", country, "/forecast_ets.RData"))
  } else {
    load(paste0("small_examples/IJF/", country, "/forecast_ets.RData"))
  }

  # # AR
  # if(!file.exists(paste0("small_examples/IJF/", country, "/forecast_ets.RData"))){
  #   set.seed(8899)
  #   fc_ets <- forecast_model(model_result_ext, exog_fill_method = "AR")
  #   save(fc_ets, file = paste0("small_examples/IJF/", country, "/forecast_ets.RData"))
  # } else {
  #   load(paste0("small_examples/IJF/", country, "/forecast_ets.RData"))
  # }

  # create inflation from HICP module
  hicp <- fc_ets$full_forecast_data %>%
    filter(na_item == "HICPlocal") %>%
    select(time, values, type)
  # first implied forecast of inflation is (HICPforecast1 - HICPobservedlast) / HICPobservedlast * 100
  firstforecast <- hicp %>% filter(type == "Forecast") %>% pull(time) %>% min()
  obsbefore <- seq(firstforecast, by = "-1 quarter", length.out = 2)[2]
  valuebefore <- hicp %>% filter(time == obsbefore & type == "Observation") %>% pull(values)
  hicp <- hicp %>% add_row(time = obsbefore, values = valuebefore, type = "Forecast")
  hicp <- hicp %>%
    arrange(type, time) %>%
    group_by(type) %>%
    mutate(Inflation = (values - lag(values, 1)) / lag(values, 1) * 100) %>%
    # remove forecast first observation again (should be NA anyway)
    ungroup() %>%
    filter(!(time == obsbefore & type == "Forecast")) %>%
    mutate(na_item = "Inflation2") %>%
    select(time, na_item, Inflation, type) %>%
    rename(values = Inflation)
  # add inflation data to forecast data (keep both Inflation and Inflation2 for comparison initially)
  fc_ets$full_forecast_data <- bind_rows(fc_ets$full_forecast_data, hicp)
  fc_ets$forecast <- fc_ets$forecast %>% add_row(dep_var = "Inflation2", central.estimate = list(hicp %>% filter(type == "Forecast") %>% select(time, values) %>% rename(Inflation2 = values)))
  fc_ets$orig_model$opts_df <- fc_ets$orig_model$opts_df %>% add_row(dependent = "Inflation2", log_opts = list(tibble(Inflation2 = "none")))
  plot(fc_ets, title = paste0("Forecast for ",country_long), linewidth = lwidth, grepl_variables = "Inflation")
  # now that have compared, replace the original inflation with the HICP one
  fc_ets$full_forecast_data <- bind_rows(fc_ets$full_forecast_data %>% filter(na_item != "Inflation"), hicp %>% mutate(na_item = "Inflation"))
  # also replace $forecast
  # identify current inflation module
  inflocation <- which(fc_ets$forecast$dep_var == "Inflation")
  # replace central.estimate object
  mintime <- fc_ets$forecast %>% filter(dep_var == "Inflation") %>% pull(central.estimate) %>% pluck(1) %>% pull(time) %>% min()
  fc_ets$forecast[inflocation, "central.estimate"][[1]] <- list(hicp %>% filter(time >= mintime) %>% select(time, values) %>% rename(Inflation = values))
  fc_ets$forecast[inflocation, "all.estimates"] <- tibble(all.estimates = list(NULL))
  # identify current inflation module
  inflocation <- which(fc_ets$orig_model$opts_df$dependent == "Inflation")
  fc_ets$orig_model$opts_df[inflocation, "log_opts"][[1]] <- list(tibble(Inflation = "none"))

  if(plot_forecasts){
    # Forecast Plot -----------------------------------------------------------

    plot(fc, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast",".pdf"), width = 12, height = 10)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc, grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected",".pdf"), width = 7, height = 5)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected_2015",".pdf"), width = 7, height = 5)


    ## ETS Plots -----------


    plot(fc_ets, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_ets",".pdf"), width = 12, height = 10)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc_ets, grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected_ets",".pdf"), width = 7, height = 5)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc_ets, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected_2015_ets",".pdf"), width = 7, height = 5)

  }

  # table for forecasts
  fc$forecast %>%
    filter(grepl(vars_to_grab, dep_var)) %>%
    select(dep_var, central.estimate) %>%
    unnest(central.estimate) %>%
    select(-dep_var) %>%
    pivot_longer(-c(time)) %>%
    drop_na %>%
    pivot_wider(id_cols = c(time), names_from = name, values_from = value) %>%
    rename(Forecast = time) %>%

    kable(format = "latex",
          booktabs = TRUE, label = paste0("tab:forecast_",country),
          caption = paste0("Forecast for selected modules for ", country_long, ".")) %>%
    kable_styling() %>%
    table_change(label = paste0("tab:forecast_",country)) %>%
    writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Forecast.tex"))

  # table for forecasts
  fc_ets$forecast %>%
    filter(grepl(vars_to_grab, dep_var)) %>%
    select(dep_var, central.estimate) %>%
    unnest(central.estimate) %>%
    select(-dep_var) %>%
    pivot_longer(-c(time)) %>%
    drop_na %>%
    pivot_wider(id_cols = c(time), names_from = name, values_from = value) %>%
    rename(Forecast = time) %>%

    kable(format = "latex",
          booktabs = TRUE, label = paste0("tab:forecast_",country),
          caption = paste0("Forecast for selected modules for ", country_long, ".")) %>%
    kable_styling() %>%
    table_change(label = paste0("tab:forecast_",country)) %>%
    writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Forecast_ets.tex"))

  # Insample Forecasting ----------------------------------------------------

  if(!file.exists(paste0("small_examples/IJF/", country, "/insample.RData"))){
    set.seed(8899)
    insample <- forecast_insample(model_result_ext, sample_share = .9, exog_fill_method = c("auto", "AR", "ets"))
    save(insample, file = paste0("small_examples/IJF/", country, "/insample.RData"))
  } else {
    load(paste0("small_examples/IJF/", country, "/insample.RData"))
  }
  if(run_insample){
    # Insample Forecasting Plots ----------------------------------------------------

    plot(insample, title = paste0("Insample Forecasting for ",country_long), linewidth = lwidth) %>%
      ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample",".pdf"), width = 10, height = 10)

    # Insample Forecasting Plots with selected variables ----------------------------------------------------

    plot(insample, grepl_variables = vars_to_grab, title = paste0("Insample Forecasting for ",country_long), linewidth = lwidth) %>%
      ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample_Selected",".pdf"), width = 7, height = 5)

    # Insample Forecasting Plots with selected variables ----------------------------------------------------

    plot(insample, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Insample Forecasting for ",country_long), linewidth = lwidth) %>%
      ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample_Selected_2015",".pdf"), width = 7, height = 5)

  }
  # # table for RMSFE
  # insample$rmsfe %>%
  #   filter(grepl(vars_to_grab, na_item)) %>%
  #   pivot_wider(id_cols = start, names_from = na_item, values_from = rmsfe) %>%
  #
  #   kable(format = "latex",
  #         booktabs = TRUE, label = paste0("tab:RMSFE_",country),
  #         caption = paste0("Root Mean Squared Forecast Error for each module for ", country_long, ".")) %>%
  #   kable_styling() %>%
  #   writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_RMSFE.tex"))


  # Insample Forecast Comparison --------------------------------------------
  if(run_fc_comparison){


    fc_comparison_base <- insample$all_models[length(insample$all_models) - 7][[1]]
    set.seed(8899)
    naive_ar <- forecast_comparison(model = fc_comparison_base, n.ahead = 8, forecast_type = "AR")
    set.seed(8899)
    naive_rw <- forecast_comparison(model = fc_comparison_base, n.ahead = 8, forecast_type = "RW")
    set.seed(8899)
    naive_ets <- forecast_comparison(model = fc_comparison_base, n.ahead = 8, forecast_type = "ets")
    set.seed(8899)
    naive_auto <- forecast_comparison(model = fc_comparison_base, n.ahead = 8, forecast_type = "auto")


    # set.seed(8899)
    # fc_comparison_AR <- forecast_model(fc_comparison_base, n.ahead = 8, exog_fill_method = "AR")
    set.seed(8899)
    fc_comparison_auto <- forecast_model(fc_comparison_base, n.ahead = 8, exog_fill_method = "auto")
    set.seed(8899)
    fc_comparison_ets <- forecast_model(fc_comparison_base, n.ahead = 8, exog_fill_method = "ets")

    insample$hist_data %>%
      rename(na_item = dep_var,
             hist = values) %>%
      right_join(naive_ar %>% select(-forecast_type), by = join_by(time, na_item)) %>%
      # calculate rmsfe
      mutate(sfe = (hist - values)^2) %>%
      group_by(na_item) %>%
      summarise(rmsfe = sqrt(mean(sfe, na.rm = TRUE))) -> rmsfe_ar_naive

    insample$hist_data %>%
      rename(na_item = dep_var,
             hist = values) %>%
      right_join(naive_rw %>% select(-forecast_type), by = join_by(time, na_item)) %>%
      # calculate rmsfe
      mutate(sfe = (hist - values)^2) %>%
      group_by(na_item) %>%
      summarise(rmsfe = sqrt(mean(sfe, na.rm = TRUE))) -> rmsfe_rw_naive

    insample$hist_data %>%
      rename(na_item = dep_var,
             hist = values) %>%
      right_join(naive_ets %>% select(-forecast_type), by = join_by(time, na_item)) %>%
      # calculate rmsfe
      mutate(sfe = (hist - values)^2) %>%
      group_by(na_item) %>%
      summarise(rmsfe = sqrt(mean(sfe, na.rm = TRUE))) -> rmsfe_ets_naive

    insample$hist_data %>%
      rename(na_item = dep_var,
             hist = values) %>%
      right_join(naive_auto %>% select(-forecast_type), by = join_by(time, na_item)) %>%
      # calculate rmsfe
      mutate(sfe = (hist - values)^2) %>%
      group_by(na_item) %>%
      summarise(rmsfe = sqrt(mean(sfe, na.rm = TRUE))) -> rmsfe_auto_naive

    # rmsfe(fc_comparison_AR, data = insample$hist_data %>% rename(na_item = dep_var)) %>%
    #   mutate(type = "AR") %>%
    rmsfe(fc_comparison_auto, data = insample$hist_data %>% rename(na_item = dep_var)) %>%
      mutate(type = "Auto") %>%
      bind_rows(rmsfe(fc_comparison_ets, data = insample$hist_data %>% rename(na_item = dep_var)) %>%
                  mutate(type = "ETS")) %>%
      bind_rows(rmsfe_ar_naive %>% mutate(type = "Naive AR")) %>%
      bind_rows(rmsfe_rw_naive %>% mutate(type = "Naive RW")) %>%
      bind_rows(rmsfe_ets_naive %>% mutate(type = "Naive ets")) %>%
      bind_rows(rmsfe_auto_naive %>% mutate(type = "Naive auto"))-> all_rsmfe

    all_rsmfe %>%
      filter(type == "Naive AR") %>%
      rename(base = rmsfe) %>%
      select(-type) %>%

      full_join(all_rsmfe, by = "na_item") %>%

      filter(grepl(vars_for_spec_table, na_item)) %>%

      mutate(comparison = rmsfe/base) %>%
      pivot_wider(id_cols = na_item, names_from = type, values_from = comparison) %>%
      summarise(across(-na_item, list(mean = ~mean(.x, na.rm = TRUE),
                                      median = ~median(.x, na.rm = TRUE))))


    # create a ggplot with the observational record from insample$hist_data
    # and all forecasts from naive_ar and naive_rw, fc_comparison_auto, fc_comparison_ets and fc_comparison_AR
    insample$hist_data %>%
      rename(na_item = dep_var) %>%
      mutate(type = "Observation") %>%
      bind_rows(naive_ar %>% select(-forecast_type) %>% mutate(type = "Naive AR")) %>%
      bind_rows(naive_rw %>% select(-forecast_type) %>% mutate(type = "Naive RW")) %>%
      bind_rows(naive_ets %>% select(-forecast_type) %>% mutate(type = "Naive ets")) %>%
      bind_rows(naive_auto %>% select(-forecast_type) %>% mutate(type = "Naive auto")) %>%
      bind_rows(extract_central_forecasts(fc_comparison_auto) %>% mutate(type = "Auto") %>% rename(values = forecast)) %>%
      bind_rows(extract_central_forecasts(fc_comparison_ets) %>% mutate(type = "ETS") %>% rename(values = forecast)) %>%
      #bind_rows(extract_central_forecasts(fc_comparison_AR) %>% mutate(type = "AR") %>% rename(values = forecast)) %>%

      filter(grepl(vars_for_spec_table, na_item)) %>%

      ggplot() +
      geom_line(aes(x = time, y = values, color = type)) +
      facet_wrap(~na_item, scale = "free_y") +
      labs(title = paste0("Insample Forecast Comparison for ",country_long),
           subtitle = "Showing the <span style = color:#440154FF>Observed</span> and <span style = color:#FDE725FF>Fitted</span> values.") +
      theme_minimal(base_size = 12) +
      theme(plot.subtitle = element_markdown()) +
      theme(legend.position = "bottom")

  }
  # Regression Summary ------------------------------------------------------

  # remove NULL results
  model_list <- model_result_ext$module_collection$model[!sapply(model_result_ext$module_collection$model, is.null)]

  model_list <- lapply(model_list, function(x){if(!is.null(x)){gets::as.lm(x)}})

  names(model_list) <- model_result_ext$module_order$dependent[!sapply(model_result_ext$module_collection$model, is.null)]

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

  # extract transformations
  # technically, the transformation of the same var could differ across modules because of different samples
  # need to ensure first the transformations are the same (otherwise cannot be same row in output latex table)
  trafo <- bind_rows(model_result_ext$opts_df %>% pull(log_opts))
  trafo <- trafo %>%
    reframe(across(everything(), ~ na.omit(unique(.x))))
  stopifnot(NROW(trafo) == 1L) # then can use same transformation in all tables
  trafo <- trafo %>% pivot_longer(everything(), names_to = "variable", values_to = "trafo")

  coef_renamed <- sapply(coef_order, trafo_fun)
  names(coef_renamed) <- coef_order # names should already be set but to be sure

  ## Regression Tables -------------------------------------------------------

  ## All variables -----------------------------------------------------------
  # To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\num{}`, call:
  #   options("modelsummary_format_numeric_latex" = "plain")
  no_of_tables <- ceiling(length(model_list)/no_modles_in_one_table)

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
      title = paste0("Final OSEM Model result for each module for ",country_long, ". Part ",i,"."),
      notes = "Quarterly Dummies, Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
      stars = TRUE
    )%>%
      kable_styling(font_size = 8)

    # this output can go straight into the latex
    table_change(table_output, label = paste0("tab:regression_summary",country,"_",i)) %>%
      writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Regression_Summary_",i,".tex"))
  }


  ## selected variables ------------------------------------------------------

  model_list[grepl(vars_to_grab, names(model_list))] -> model_list_temp
  models_renamed <- sapply(names(model_list_temp), function(x) paste0(trafo %>% filter(variable == x) %>% pull(trafo), "(", x, ")"))
  names(model_list_temp) <- models_renamed

  table_output <- modelsummary::modelsummary(
    model_list_temp,
    #coef_omit = "iis|sis|q_[0-9]+",
    coef_map = coef_renamed,
    gof_omit = "R",
    output = "latex",
    title = paste0("Final OSEM Model result for selected modules for ",country_long, "."),
    notes = "Quarterly Dummies, Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
    stars = TRUE
  ) %>%
    kable_styling(font_size = 8)

  # this output can go straight into the latex
  table_change(table_output, label = paste0("tab:regression_summary",country,"_",i)) %>%
    writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Regression_Summary_selected.tex"))


  # Diagnostics ------------------------------------------------------------

  options(knitr.kable.NA = '')
  diagnostics_model(model_result_ext) %>%
    rename(Module = module) %>%

    kable(format = "latex", booktabs = TRUE, digits = 3, label = paste0(country,"_diagnostics"),
          caption = paste0("Diagnostic results for each estimated module for ", country_long, ".")) %>%
    kable_styling() %>%
    writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Diagnostics.tex"))

  # diagnostics_model(model_result_ext) %>%
  #   mutate(across(c(AR,ARCH, `Super Exogeneity`), ~ round(.,4))) %>%
  #   DT::datatable() %>%
  #   DT::formatStyle(columns = c("AR", "ARCH", "Super Exogeneity"),
  #                   backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
  #   DT::formatRound(columns = c("AR", "ARCH", "Share of Indicators"),
  #                   digits = 4)

  if(run_network){
    set.seed(1234)
    model_result_ext %>%
      network(layout = "fr") -> p

    ggsave(p, width = 8, height = 10, file = paste0("small_examples/IJF/figures_overleaf/", country, "_Network",".pdf"), bg = "white")
  }


  # Summary Statistics ------------------------------------------------------

  summary_stats <- datasummary_skim(model_result_ext$processed_input_data %>%
                                      pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") %>%
                                      as.data.frame() %>%
                                      arrange(time), histogram = FALSE, output = "data.frame")

  time_sample <- model_result_ext$processed_input_data %>% summarise(min = min(time, na.rm = TRUE), max = max(time, na.rm = TRUE), .by = na_item) %>%
    # turn date into Q and year
    mutate(min = paste0("Q",quarter(min, type = "quarter"),"-", year(min)),
           max = paste0("Q",quarter(max, type = "quarter"),"-", year(max))) %>%
    mutate(`Sample Period` = paste0(min, " - ", max),
           min = NULL,
           max = NULL)

  summary_stats %>%
    rename(na_item = " ") %>%
    full_join(time_sample, by = "na_item") %>%
    relocate(`Sample Period`, .after = na_item) %>%
    rename(" " = na_item) %>%
    select(-`Missing (%)`) -> summary_stats_ready

  kable(summary_stats_ready, format = "latex", longtable = TRUE, booktabs = TRUE, caption = paste0("Summary Statistics for ", country_long, ".")) %>%
    kable_styling(font_size = 7) %>%
    table_change(., label = paste0("tab:summarystats_",country)) %>%
    writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Summary_Statistics.tex"))




  # Scenario ----------------------------------------------------------------
  if(run_scenario){
    base_fc <- forecast_model(model_result_ext_sel, exog_fill_method = "ets")
    #base_fc <- forecast_model(model_result_ext_sel, exog_fill_method = "AR")

    base_fc$exog_data_nowcast %>%
      mutate(PriceETS = PriceETS + 100,
             HICP_AviaInt = HICP_AviaInt * 1.1,
             HICP_Electricity = HICP_Electricity * 1.1) -> exog_data_new


    scen_fc <- forecast_model(model_result_ext_sel, exog_predictions = exog_data_new, exog_fill_method = "AR")

    base_df <- plot(base_fc, grepl_variables = "EmiCO2ElecHeat", return.data = TRUE)
    scen_df <- plot(scen_fc, grepl_variables = "EmiCO2ElecHeat", return.data = TRUE)

    plot(base_fc, grepl_variables = "EmiCO2Total")
    plot(scen_fc, grepl_variables = "EmiCO2Total")


    base_df %>%
      select(time, base = values) %>%
      bind_cols(scen_df %>%
                  select(scen = values)) %>%
      mutate(diff = base - scen) %>%
      summarise(sum(base, na.rm = TRUE),
                sum(diff, na.rm = TRUE))
  }
  # Diagnostic change in inventories ----------------------------------------
  # variables of interest (not modelled, summarised as DInventories in our model):
  # Eurostat: P52 = changes in inventories, P53 = acquisition less disposal of valuables, YA0 = stat discrepancy expenditure approach
  # not all series are available for all countries and years; add them up, remove NAs
  # measured either in current prices (million euros) or as percentage of GDP, we focus on the former; can rescale later
  if(run_inventory_diagnostics){
    p52_p53 <- eurostat::get_eurostat("namq_10_gdp", filters = list(geo = country, na_item = "P52_P53", unit = "CP_MEUR", s_adj = "NSA")) %>%
      select(time, values) %>%
      rename(p52_p53 = values)
    stopifnot(sum(duplicated(p52_p53$time)) == 0)
    tryCatch(
      {
        ya0 <- eurostat::get_eurostat("namq_10_gdp", filters = list(geo = country, na_item = "YA0", unit = "CP_MEUR", s_adj = "NSA"), cache = FALSE) %>%
          select(time, values) %>%
          rename(ya0 = values)
      },
      error = function(e) {
        ya0 <- data.frame(time = as.Date("1900-01-01"), ya0 = NA)
      }
    )
    stopifnot(sum(duplicated(ya0$time)) == 0)
    DInventories_data <- full_join(x = p52_p53, y = ya0, by = "time") %>%
      mutate(DInventories = rowSums(across(c(p52_p53, ya0)), na.rm = TRUE)) %>%
      # replace with NA if both missing (if don't do this step, get 0 -> misleading)
      mutate(DInventories = if_else(is.na(p52_p53) & is.na(ya0), NA, DInventories)) %>%
      mutate(type = "Data")
    # obtain actual GDP
    gdp <- eurostat::get_eurostat("namq_10_gdp", filters = list(geo = country, na_item = "B1GQ", unit = "CP_MEUR", s_adj = "NSA")) %>%
      select(time, values) %>%
      rename(GDP = values)
    DInventories_data <- left_join(DInventories_data, gdp, by = "time") %>%
      mutate(DInventories_pct = DInventories / GDP * 100) %>%
      select(time, type, DInventories, DInventories_pct) %>%
      pivot_longer(cols = c(DInventories, DInventories_pct), names_to = "unit", values_to = "DInventories") %>%
      mutate(unit = case_when(unit == "DInventories" ~ "Million EUR (current prices)", unit == "DInventories_pct" ~ "% of GDP"))
    # obtain DInventories and GDP model values and forecasts (fc_ext should be created above)
    fc_ext <- forecast_model(model_result_ext, exog_fill_method = "ets")
    DInventories_model <- fc_ext$full_forecast_data %>%
      filter(na_item %in% c("DInventories", "GDPExpenditure")) %>%
      filter(type %in% c("Forecast", "Insample Fit", "Nowcast")) %>%
      select(time, na_item, values, type) %>%
      pivot_wider(id_cols = c(time, type), names_from = na_item, values_from = values) %>%
      mutate(DInventories_pct = DInventories / GDPExpenditure * 100) %>%
      select(time, type, DInventories, DInventories_pct) %>%
      drop_na()
    # with nowcasts may now have duplicates
    # stopifnot(sum(duplicated(DInventories_model$time)) == 0)
    DInventories_model <- DInventories_model %>%
      pivot_longer(cols = c(DInventories, DInventories_pct), names_to = "unit", values_to = "DInventories") %>%
      mutate(unit = case_when(unit == "DInventories" ~ "Million EUR (current prices)", unit == "DInventories_pct" ~ "% of GDP"))
    # combine data and model values for comparison
    DInventories_comp <- bind_rows(DInventories_model, DInventories_data) %>%
      arrange(time) %>%
      filter(cumsum(!is.na(DInventories)) > 0)

    colors <- c(
      "Forecast/Assumption of\nExogenous Variables" = "#31688EFF",
      "Forecast" = "#3B528BFF",
      "Insample Fit" = "#FDE725FF",
      "Observation" = "#440154FF",
      "Nowcast" = "#35B779FF"
    )

    DInventories_comp %>%
      filter(unit == "% of GDP") %>%
      mutate(type = case_when(type == "Data" ~ "Observation", TRUE ~ type)) %>%
      ggplot() +
      geom_line(aes(x = time, y = DInventories, color = type), linewidth = lwidth) +
      labs(x = NULL, y = "Change in Inventories (% of GDP)", title = paste0("Inventory Diagnostic Plot for ", country_long), subtitle = fc_subtitle) +
      #facet_grid(rows = vars(unit), cols = NULL, scales = "free") +
      scale_colour_manual(values = colors) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     plot.subtitle = element_markdown()) -> DInventories_plot

    ggsave(DInventories_plot, filename = paste0("small_examples/IJF/figures_overleaf/", country, "_DInventories",".pdf"), width = 7, height = 5)
  }



}






# Combine all tex files ---------------------------------------------------

# Specify the directory containing .tex files
tex_dir <- "small_examples/IJF/tables_overleaf"

# Specify the output file
output_file <- "small_examples/IJF/tables_overleaf/appendix_tables.tex"

# Get all .tex files in the directory (excluding the output file itself)
tex_files <- list.files(tex_dir, pattern = "\\.tex$", full.names = TRUE, recursive = FALSE)
tex_files <- tex_files[basename(tex_files) != basename(output_file)]
tex_files_maintext <- tex_files[grepl(main_country, tex_files)]
tex_files_maintext <- tex_files_maintext[!grepl("Summary_[0-9]+", tex_files_maintext)]
tex_files <- setdiff(tex_files, tex_files_maintext)

# Create the combined .tex file
file_conn <- file(output_file, "w")

# Loop through each .tex file and copy its content
for (tex_file in tex_files) {
  # Write a comment indicating the start of the file content
  writeLines(paste0("% ---- Start of ", basename(tex_file), " ----"), file_conn)

  # Read the content of the .tex file
  tex_content <- readLines(tex_file)

  # once finished with reading the content, move the tex_file into the sub-folder "done"
  file.rename(tex_file, paste0("small_examples/IJF/tables_overleaf/done/", basename(tex_file)))

  # Write the content to the combined file
  writeLines(tex_content, file_conn)

  # Write a comment indicating the end of the file content
  writeLines(paste0("% ---- End of ", basename(tex_file), " ----\n"), file_conn)
}

# Close the file connection
close(file_conn)

cat("Combined file created with content copied:", output_file, "\n")



# Combine all figures -----------------------------------------------------




# Output file
output_file <- "small_examples/IJF/figures_overleaf/figures_appendix.tex"

# Open connection to the output file
file_conn <- file(output_file, "w")

# Loop through each country and write the LaTeX code
for (country in all_countries[!grepl(main_country, all_countries)]) {
  # Define the LaTeX code for this country
  country_code <- paste0("
  \\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Network.pdf}
    \\caption{Network graph for ",country,". The graph also indicates which variables were selected and which were dropped during model selection.}
    \\label{fig:", country, "_network}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Model.pdf}
    \\caption{In-sample model fit for ",country,".}
    \\label{fig:", country, "_entire_model}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Model_Selected.pdf}
    \\caption{In-sample model fit for ",country," for selected variables.}
    \\label{fig:", country, "_selected_model}
\\end{figure}

%\\begin{figure}
%    \\centering
%    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Model_Selected_2015.pdf}
%    \\caption{In-sample model fit for ",country," for selected variables. Data shown from 2015 onwards.}
%    \\label{fig:", country, "_selected_model_2015}
%\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast.pdf}
    \\caption{Forecast for ",country," for selected variables.}
    \\label{fig:", country, "_entire_forecast}
\\end{figure}


\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast_ets.pdf}
    \\caption{Forecast for ",country," for selected variables using an AR(4) Forecast with Indicator Saturation.}
    \\label{fig:", country, "_entire_forecast_ets}
\\end{figure}

%\\begin{figure}
%    \\centering
%    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast_Selected.pdf}
% \\caption{Insample Forecast for ",country,". Data shown from 2015 onwards.}
%    \\label{fig:", country, "_selected_forecast}
%\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast_Selected_2015.pdf}
    \\caption{Forecast for ",country," for selected variables. Data shown from 2015 onwards.}
    \\label{fig:", country, "_selected_forecast_2015}
\\end{figure}

%\\begin{figure}
%    \\centering
%    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Insample.pdf}
%    \\caption{Insample Forecast for ",country,".}
%    \\label{fig:", country, "_insample_forecast}
%\\end{figure}


\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Insample_Selected_2015.pdf}
    \\caption{Insample Forecast for ",country," for selected variables. Data shown from 2015 onwards.}
    \\label{fig:", country, "_insample_selected_forecast_2015}
\\end{figure}
")

  # Write the LaTeX code to the file
  writeLines(country_code, file_conn)
}

# Close the file connection
close(file_conn)

cat("Figures LaTeX file generated:", output_file, "\n")




# bei estimated equations machen wir f() und bei identities machen wir mit + oder den jeweiligen operatoren --> DONE
# raw data series --> DROPPED
# why is a lot of super.exog NA? --> DONE
# variable table
# check network graph - real VA and selected --> DONE
# fix insample forecasting --> DONE
# legend --> DONE
# take away the trend --> DONE

# AR Forecasts
# Summary stats tables
# Policy Example
# IndProd
# keep argument for ETS --> DONE
# change the central.estimate from forecast_model
# check the insample results --> DONE
# bring back documentation of use_logs --> DONE
# step-wise indicator selection - first STEPS then INDICATORS --> DNOE
# make linewidth smaller --> DONE
# Mean Absolute Percentage Error MAPE
# log transformation in Regression Summaries --> DONE
# RMSE is for different forecasts --> DONE
# AR model --> same sample as the model --> DONE
# Random Walk from final value --> DONE



# summary statistics and specification tables --> DONE
# style the inventories plot --> DONE
# take out inflation
# scenario?
# restructuring the paper --> JONAS
# rerun the inventories with AR --> DONE
# push the latest model
# fix Figure 1 --> JONAS
# take out DFlator
# ets and auto exog for comparison

