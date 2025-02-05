library(tidyverse)
library(kableExtra)
library(osem)
library(ggtext)
library(modelsummary)

vars_to_grab <- "ElectrCons|EmiCO2RoaTra|EmiCO2ManInd|EmiCO2ElecHeat|RealVAIndustry|RealConsHH"
vars_for_spec_table <- paste0(vars_to_grab, "|VAIndustry|HICP|CapForm|CapFormHH")
no_modles_in_one_table <- 5

# define the main country to leave out of the appendix
main_country <- "DE"
all_countries <- c("DE","AT","FR","DK")

# control what to run right now
run_modelplots <- FALSE
run_forecasts <- TRUE
run_insample <- FALSE
run_inventory_diagnostics <- FALSE
run_network <- FALSE
run_scenario <- FALSE

lwidth <- 0.75

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


  # Specification Table -----------------------------------------------------

  if(!file.exists(paste0("small_examples/IJF/tables_overleaf/general/Specification for all countries.tex"))){
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
      kable_styling() %>%
      kable_styling(font_size = 8) %>%
      column_spec(4, width = "10cm") %>%
      writeLines(paste0("small_examples/IJF/tables_overleaf/general/Specification for all countries.tex"))


    full_table %>%
      filter(grepl(vars_for_spec_table, Dependent)) %>%


      kable(format = "latex", booktabs = TRUE, label = "spec_subset",
            caption = "Selected specification of individual modules and their linkages. All equations are specified to potentially also include autoregressive lags. Functions specified as f() are AR Models. The full model specification table is available in the Appendix.") %>%
      kable_styling() %>%
      kable_styling(font_size = 8) %>%
      column_spec(4, width = "7cm") %>%
      writeLines(paste0("small_examples/IJF/tables_overleaf/general/Specification selected for all countries.tex"))


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
  if(run_forecasts){

    set.seed(8899)
    fc_ext <- forecast_model(model_result_ext, exog_fill_method = "auto")


    # Forecast Plot -----------------------------------------------------------

    plot(fc_ext, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast",".pdf"), width = 12, height = 10)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc_ext, grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected",".pdf"), width = 7, height = 5)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc_ext, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected_2015",".pdf"), width = 7, height = 5)

    # table for forecasts
    fc_ext$forecast %>%
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


    # AR ----------------------------------------------------------------------
    set.seed(8899)
    fc_ext_ar <- forecast_model(model_result_ext, exog_fill_method = "AR")

    plot(fc_ext_ar, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_AR",".pdf"), width = 12, height = 10)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc_ext_ar, grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected_AR",".pdf"), width = 7, height = 5)

    # Forecast Plot with selected variables -----------------------------------------------------------
    plot(fc_ext_ar, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long), linewidth = lwidth) +
      labs(subtitle = fc_subtitle) +
      theme(plot.subtitle = element_markdown()) -> p
    ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected_2015_AR",".pdf"), width = 7, height = 5)

    # table for forecasts
    fc_ext_ar$forecast %>%
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
      writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Forecast_AR.tex"))

  }


  # Insample Forecasting ----------------------------------------------------
  if(run_insample){
    if(!file.exists(paste0("small_examples/IJF/", country, "/insample.RData"))){
      set.seed(8899)
      insample <- forecast_insample(model_result_ext, sample_share = .96, exog_fill_method = c("auto"))
      save(insample, file = paste0("small_examples/IJF/", country, "/insample.RData"))
    } else {
      load(paste0("small_examples/IJF/", country, "/insample.RData"))
    }

    # Insample Forecasting Plots ----------------------------------------------------

    plot(insample, title = paste0("Insample Forecasting for ",country_long), linewidth = lwidth) %>%
      ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample",".pdf"), width = 7, height = 5)

    # Insample Forecasting Plots with selected variables ----------------------------------------------------

    plot(insample, grepl_variables = vars_to_grab, title = paste0("Insample Forecasting for ",country_long), linewidth = lwidth) %>%
      ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample_Selected",".pdf"), width = 7, height = 5)

    # Insample Forecasting Plots with selected variables ----------------------------------------------------

    plot(insample, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Insample Forecasting for ",country_long), linewidth = lwidth) %>%
      ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample_Selected_2015",".pdf"), width = 7, height = 5)


    # table for RMSFE
    insample$rmsfe %>%
      filter(grepl(vars_to_grab, na_item)) %>%
      pivot_wider(id_cols = start, names_from = na_item, values_from = rmsfe) %>%

      kable(format = "latex",
            booktabs = TRUE, label = paste0("tab:RMSFE_",country),
            caption = paste0("Root Mean Squared Forecast Error for each module for ", country_long, ".")) %>%
      kable_styling() %>%
      writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_RMSFE.tex"))




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

  coef_renamed <- coef_order %>%
    str_replace_all("ln\\.","ln(")
  coef_renamed[grepl("ln\\(", coef_renamed)] <- paste0(coef_renamed[grepl("ln\\(", coef_renamed)], ")")
  names(coef_renamed) <- coef_order

  ## Regression Tables -------------------------------------------------------

  ## All variables -----------------------------------------------------------
  # To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\num{}`, call:
  #   options("modelsummary_format_numeric_latex" = "plain")
  no_of_tables <- ceiling(length(model_list)/no_modles_in_one_table)

  for(i in 1:no_of_tables){
    # i = 1
    model_list[cut(1:length(model_list), breaks = no_of_tables, labels = FALSE) == i] -> model_list_temp
    models_renamed <- paste0("ln(", names(model_list_temp), ")")
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
  models_renamed <- paste0("ln(", names(model_list_temp), ")")
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
                                      as.data.frame(), histogram = FALSE, output = "latex_tabular")

  table_change(summary_stats, label = paste0("tab:summarystats_",country)) %>%
    writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Summary_Statistics.tex"))




  # Scenario ----------------------------------------------------------------
  if(run_scenario){
    base_fc <- forecast_model(model_result_ext_sel, exog_fill_method = "auto")
    #base_fc <- forecast_model(model_result_ext_sel, exog_fill_method = "AR")

    base_fc$exog_data_nowcast %>%
      mutate(PriceETS = PriceETS + 100) -> exog_data_new


    scen_fc <- forecast_model(model_result_ext_sel, exog_predictions = exog_data_new)

    base_df <- plot(base_fc, grepl_variables = "EmiCO2Total", return.data = TRUE)
    scen_df <- plot(scen_fc, grepl_variables = "EmiCO2Total", return.data = TRUE)

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
      mutate(unit = case_when(unit == "DInventories" ~ "CP_MEUR", unit == "DInventories_pct" ~ "% of GDP"))
    # obtain DInventories and GDP model values and forecasts (fc_ext should be created above)
    fc_ext <- forecast_model(model_result_ext, exog_fill_method = "auto")
    DInventories_model <- fc_ext$full_forecast_data %>%
      filter(na_item %in% c("DInventories", "GDPExpenditure")) %>%
      filter(type %in% c("Forecast", "Insample Fit")) %>%
      select(time, na_item, values, type) %>%
      pivot_wider(id_cols = c(time, type), names_from = na_item, values_from = values) %>%
      mutate(DInventories_pct = DInventories / GDPExpenditure * 100) %>%
      select(time, type, DInventories, DInventories_pct) %>%
      drop_na()
    stopifnot(sum(duplicated(DInventories_model$time)) == 0)
    DInventories_model <- DInventories_model %>%
      pivot_longer(cols = c(DInventories, DInventories_pct), names_to = "unit", values_to = "DInventories") %>%
      mutate(unit = case_when(unit == "DInventories" ~ "CP_MEUR", unit == "DInventories_pct" ~ "% of GDP"))
    # combine data and model values for comparison
    DInventories_comp <- bind_rows(DInventories_model, DInventories_data) %>%
      arrange(time) %>%
      filter(cumsum(!is.na(DInventories)) > 0)
    ggplot(DInventories_comp) +
      geom_line(aes(x = time, y = DInventories, color = type)) +
      facet_grid(rows = vars(unit), cols = NULL, scales = "free") -> DInventories_plot

    ggsave(DInventories_plot, filename = paste0("small_examples/IJF/figures_overleaf/internal/", country, "_DInventories",".pdf"), width = 7, height = 5)
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

%\\begin{figure}
%    \\centering
%    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast_Selected.pdf}
\\caption{Insample Forecast for ",country,". Data shown from 2015 onwards.}
%    \\label{fig:", country, "_selected_forecast}
%\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast_Selected_2015.pdf}
    \\caption{Forecast for ",country," for selected variables. Data shown from 2015 onwards.}
    \\label{fig:", country, "_selected_forecast_2015}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Insample.pdf}
    \\caption{Insample Forecast for ",country,".}
    \\label{fig:", country, "_insample_forecast}
\\end{figure}


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
# bring back documentation of use_logs
# step-wise indicator selection - first STEPS then INDICATORS
# make linewidth smaller
# Mean Absolute Percentage Error MAPE
# log transformation in Regression Summaries


load(paste0("./small_examples/IJF/DE/model_sel.RData"))
a <- forecast_comparison(model = model_result_ext_sel, n.ahead = 10, forecast_type = "AR")
b <- forecast_comparison(model = model_result_ext_sel, n.ahead = 10, forecast_type = "RW")


