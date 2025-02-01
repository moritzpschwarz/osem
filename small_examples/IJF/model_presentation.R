library(tidyverse)
library(kableExtra)
library(osem)
library(ggtext)
library(modelsummary)

vars_to_grab <- "ElectrCons|EmiCO2RoaTra|EmiCO2ManInd|EmiCO2ElecHeat|RealVAIndustry|RealConsHH"
vars_for_spec_table <- paste0(vars_to_grab, "|VAIndustry|HICP|CapForm|CapFormHH")
no_modles_in_one_table <- 5


for(country in c("DE","AT","FR","DK")){

  print(country)
  # country = "DE"

  # to do:
  # add equation from Felix --> DONE
  # add environmental equations --> DONE
  # transfer to other countries --> DONE
  # change to EDGAR v9 --> DONE
  # change environmental variables to real values --> DONE
  # fix Flights and Inflation


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
            caption = "Specification of individual modules and their linkages. All equations are specified to potentially also include autoregressive lags. Functions specified as f() are AR Models. The full model specification table is available in the Appendix.") %>%
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

  # ## Huge Model plot ---------------------------------------------------------
  #
  # plot(model_result_ext, title = paste0("OSEM Model Output for ",country_long)) +
  #   labs(subtitle = mod_subtitle) +
  #   theme(plot.subtitle = element_markdown()) -> p
  # ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Model",".pdf"), width = 12, height = 10)
  #
  # ## Model plot with selected variables ---------------------------------------------------------
  # plot(model_result_ext, grepl_variables = vars_to_grab, title = paste0("OSEM Model Output for ",country_long)) +
  #   labs(subtitle = mod_subtitle) +
  #   theme(plot.subtitle = element_markdown()) -> p
  # ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Model_Selected",".pdf"), width = 7, height = 5)
  #
  #
  # # Same Model Plot from 2015 -----------------------------------------------
  # plot(model_result_ext, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("OSEM Model Output for ",country_long)) +
  #   labs(subtitle = mod_subtitle) +
  #   theme(plot.subtitle = element_markdown()) -> p
  # ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Model_Selected_2015",".pdf"), width = 7, height = 5)
  #
  #
  # # Forecast ----------------------------------------------------------------
  # set.seed(8899)
  # fc_ext <- forecast_model(model_result_ext, exog_fill_method = "auto")
  #
  # # Forecast Plot -----------------------------------------------------------
  #
  # plot(fc_ext, title = paste0("Forecast for ",country_long)) +
  #   labs(subtitle = fc_subtitle) +
  #   theme(plot.subtitle = element_markdown()) -> p
  # ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast",".pdf"), width = 12, height = 10)
  #
  # # Forecast Plot with selected variables -----------------------------------------------------------
  # plot(fc_ext, grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long)) +
  #   labs(subtitle = fc_subtitle) +
  #   theme(plot.subtitle = element_markdown()) -> p
  # ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected",".pdf"), width = 7, height = 5)
  #
  # # Forecast Plot with selected variables -----------------------------------------------------------
  # plot(fc_ext, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Forecast for ",country_long)) +
  #   labs(subtitle = fc_subtitle) +
  #   theme(plot.subtitle = element_markdown()) -> p
  # ggsave(p,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Forecast_Selected_2015",".pdf"), width = 7, height = 5)
  #
  # # Insample Forecasting ----------------------------------------------------
  # set.seed(8899)
  # insample <- forecast_insample(model_result_ext, sample_share = .96, exog_fill_method = c("auto"))
  #
  # # Insample Forecasting Plots ----------------------------------------------------
  #
  # plot(insample, title = paste0("Insample Forecasting for ",country_long)) %>%
  #   ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample",".pdf"), width = 7, height = 5)
  #
  # # Insample Forecasting Plots with selected variables ----------------------------------------------------
  #
  # plot(insample, grepl_variables = vars_to_grab, title = paste0("Insample Forecasting for ",country_long)) %>%
  #   ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample_Selected",".pdf"), width = 7, height = 5)
  #
  # # Insample Forecasting Plots with selected variables ----------------------------------------------------
  #
  # plot(insample, first_date = "2015-01-01", grepl_variables = vars_to_grab, title = paste0("Insample Forecasting for ",country_long)) %>%
  #   ggsave(.,filename = paste0("small_examples/IJF/figures_overleaf/", country, "_Insample_Selected_2015",".pdf"), width = 7, height = 5)

  # Regression Summary ------------------------------------------------------

  # remove NULL results
  model_list <- model_result_ext$module_collection$model[!sapply(model_result_ext$module_collection$model, is.null)]

  model_list <- lapply(model_list, function(x){if(!is.null(x)){gets::as.lm(x)}})

  names(model_list) <- model_result_ext$module_order$dependent[!sapply(model_result_ext$module_collection$model, is.null)]

  table_change <- function(input_table, label){

    # insert caption
    tab <- gsub("\\begin{table}",
                paste0("\\begin{table}\n\\label{",label,"}"), input_table, fixed = TRUE)

    # insert resizebox
    tab <- gsub("\\begin{tabular}",
                "\\resizebox{\\textwidth}{!}{\\begin{tabular}", tab, fixed = TRUE)

    # fixing the footer
    tab <- gsub("\\end{tabular}",
                "\\end{tabular}}",tab, fixed = TRUE)

    return(tab)
  }

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


  ## Regression Tables -------------------------------------------------------

  ## All variables -----------------------------------------------------------
  # To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\num{}`, call:
  #   options("modelsummary_format_numeric_latex" = "plain")
  no_of_tables <- ceiling(length(model_list)/no_modles_in_one_table)

  for(i in 1:no_of_tables){
    # i = 1
    model_list[cut(1:length(model_list), breaks = no_of_tables, labels = FALSE) == i] -> model_list_temp

    table_output <- modelsummary::modelsummary(
      model_list_temp,
      #coef_omit = "iis|sis|q_[0-9]+",
      coef_map = coef_order,
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

  table_output <- modelsummary::modelsummary(
    model_list_temp,
    #coef_omit = "iis|sis|q_[0-9]+",
    coef_map = coef_order,
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

  # set.seed(1234)
  # model_result_ext %>%
  #   network(layout = "fr") -> p
  #
  # ggsave(p, width = 8, height = 10, file = paste0("small_examples/IJF/figures_overleaf/", country, "_Network",".pdf"), bg = "white")



  #insample <- forecast_insample(model_result_ext, sample_share = .99, exog_fill_method = c("AR","auto"))


  # Summary Statistics ------------------------------------------------------

  summary_stats <- datasummary_skim(model_result_ext$processed_input_data %>%
                                      pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") %>%
                                      as.data.frame(), histogram = FALSE, output = "latex_tabular")

  table_change(summary_stats, label = paste0("tab:summarystats_",country)) %>%
    writeLines(paste0("small_examples/IJF/tables_overleaf/", country, "_Summary_Statistics.tex"))




  # Scenario ----------------------------------------------------------------

  base_fc <- forecast_model(model_result_ext_sel, exog_fill_method = "auto")

  base_fc$exog_data_nowcast %>%
    mutate(PriceETS = PriceETS + 100) -> exog_data_new


  scen_fc <- forecast_model(model_result_ext_sel, exog_predictions = exog_data_new)

  base_df <- plot(base_fc, grepl_variables = "EmiCO2Total", return.data = TRUE)
  scen_df <- plot(scen_fc, grepl_variables = "EmiCO2Total", return.data = TRUE)


  base_df %>%
    select(time, base = values) %>%
    bind_cols(scen_df %>%
                select(scen = values)) %>%
    mutate(diff = base - scen) %>%
    summarise(sum(base, na.rm = TRUE),
              sum(diff, na.rm = TRUE))

  # Diagnostic change in inventories ----------------------------------------
  # variables of interest (not modelled, summarised as DInventories in our model):
  # Eurostat: P52 = changes in inventories, P53 = acquisition less disposal of valuables, YA0 = stat discrepancy expenditure approach
  # not all series are available for all countries and years; add them up, remove NAs
  # measured either in current prices (million euros) or as percentage of GDP, we focus on the former; can rescale later
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

}





# Combine all tex files ---------------------------------------------------

# Specify the directory containing .tex files
tex_dir <- "small_examples/IJF/tables_overleaf"

# Specify the output file
output_file <- "small_examples/IJF/tables_overleaf/general/combined_inputs.tex"

# Get all .tex files in the directory (excluding the output file itself)
tex_files <- list.files(tex_dir, pattern = "\\.tex$", full.names = TRUE, recursive = FALSE)
tex_files <- tex_files[basename(tex_files) != basename(output_file)]

# Create the combined .tex file
file_conn <- file(output_file, "w")

# Loop through each .tex file and copy its content
for (tex_file in tex_files) {
  # Write a comment indicating the start of the file content
  writeLines(paste0("% ---- Start of ", basename(tex_file), " ----"), file_conn)

  # Read the content of the .tex file
  tex_content <- readLines(tex_file)

  # Write the content to the combined file
  writeLines(tex_content, file_conn)

  # Write a comment indicating the end of the file content
  writeLines(paste0("% ---- End of ", basename(tex_file), " ----\n"), file_conn)
}

# Close the file connection
close(file_conn)

cat("Combined file created with content copied:", output_file, "\n")



# Combine all figures -----------------------------------------------------

# List of countries
countries <- c("AT", "DK", "DE", "FR")

# Output file
output_file <- "small_examples/IJF/figures_overleaf/figures.tex"

# Open connection to the output file
file_conn <- file(output_file, "w")

# Loop through each country and write the LaTeX code
for (country in countries) {
  # Define the LaTeX code for this country
  country_code <- paste0("
  \\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Network.pdf}
    \\caption{}
    \\label{fig:", country, "_network}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Model.pdf}
    \\caption{}
    \\label{fig:", country, "_entire_model}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Model_Selected.pdf}
    \\caption{}
    \\label{fig:", country, "_selected_model}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Model_Selected_2015.pdf}
    \\caption{}
    \\label{fig:", country, "_selected_model_2015}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast.pdf}
    \\caption{}
    \\label{fig:", country, "_entire_forecast}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast_Selected.pdf}
    \\caption{}
    \\label{fig:", country, "_selected_forecast}
\\end{figure}

\\begin{figure}
    \\centering
    \\includegraphics[width = \\textwidth]{figures/figures_Jan25/", country, "_Forecast_Selected_2015.pdf}
    \\caption{}
    \\label{fig:", country, "_selected_forecast_2015}
\\end{figure}")

  # Write the LaTeX code to the file
  writeLines(country_code, file_conn)
}

# Close the file connection
close(file_conn)

cat("Figures LaTeX file generated:", output_file, "\n")




# bei estimated equations machen wir f() und bei identities machen wir mit + oder den jeweiligen operatoren
# raw data series
# why is a lot of super.exog NA? --> DONE
# variable table
# check network graph - real VA and selected --> DONE
# fix insample forecasting --> DONE
# legend --> DONE
# take away the trend
