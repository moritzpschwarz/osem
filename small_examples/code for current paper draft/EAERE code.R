library(tidyverse)
library(kableExtra)
library(ggtext)
library(modelsummary)
devtools::load_all()

dict %>%
  bind_rows(tibble(
    model_varname = "IndProd", # this is free to choose but must be unique
    full_name = "An index of Industrial Production",
    database  = "eurostat",
    variable_code = "PRD", # in this case use the bt_indicator information here
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt", # here we specify what the column with the variables is called
    freq = "q", # for quarterly data, 'm' would be monthly
    geo = "AT",
    unit = "I15", # for index of 2015 = 100
    s_adj = "NSA", # not seasonally adjusted
    nace_r2 = "B-D")) %>%


  bind_rows(tibble(
    model_varname = "Slaughter", # this is free to choose but must be unique
    full_name = "Slaughterings",
    database  = "eurostat",
    variable_code = "SL", # in this case use the bt_indicator information here
    dataset_id = "apro_mt_pwgtm",
    var_col = "meatitem", # here we specify what the column with the variables is called
    freq = "m", # for quarterly data, 'm' would be monthly
    geo = "AT",
    unit = "THS_T",
    meat = "B1000")) %>%


  bind_rows(tibble(
    model_varname = "Consumption", # this is free to choose but must be unique
    full_name = "Final consumption expenditure",
    database  = "eurostat",
    variable_code = "P3", # in this case use the bt_indicator information here
    dataset_id = "namq_10_gdp",
    var_col = "na_item", # here we specify what the column with the variables is called
    freq = "q", # for quarterly data, 'm' would be monthly
    geo = "AT",
    unit = "CP_MEUR",
    s_adj = "NSA")) %>%


  # bind_rows(tibble(
  #   model_varname = "GValueAddAgri", # this is free to choose but must be unique
  #   full_name = "Gross Value Added in Agriculture",
  #   database  = "eurostat",
  #   variable_code = "B1G", # in this case use the bt_indicator information here
  #   dataset_id = "namq_10_a10",
  #   var_col = "na_item", # here we specify what the column with the variables is called
  #   freq = "q", # for quarterly data, 'm' would be monthly
  #   geo = "AT",
  #   unit = "CP_MEUR", # for index of 2015 = 100
  #   s_adj = "NSA", # not seasonally adjusted
  #   nace_r2 = "A"))

  return() -> new_dict


forecast_method = "AR"
data_path <- "small_examples/code for current paper draft/October 2024/" # afterwards country will be created
fig_path <- "small_examples/code for current paper draft/October 2024/figures/"

#,"DE","FR"
for(country in c("AT")){

  new_dict %>%
    mutate(geo = country) -> new_dict

  specification <- dplyr::tibble(
    type = c(
      "n",
      "n",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "EmiCO2Industry",
      "IndProd",
      "EmiCO2Combustion",
      "EmiCH4Livestock",
      "Consumption"
    ),
    independent = c(
      "HICP_Gas + HICP_Electricity + IndProd",
      "GDP + Export",
      "Consumption + HICP_Electricity + EmiCO2Industry",
      "Consumption + Export + Slaughter + GValueAddAgri",
      "FinConsExpGov + HICP_Gas + HICP_Electricity + GDP"
    )
  )
  specification


  specification %>%
    select(-type) %>%
    rename_with(str_to_title) %>%
    kable(format = "latex", booktabs = TRUE) %>%
    kable_styling()


  model <- run_model(specification = specification,
                     dictionary = new_dict,
                     inputdata_directory = paste0(data_path,country),
                     primary_source = "local",
                     save_to_disk = paste0(data_path,country,"/EAERE_data_updated.xlsx"),

                     present = FALSE,
                     quiet = FALSE,
                     selection.tpval = 0.001,
                     constrain.to.minimum.sample = FALSE)




  model_list <- lapply(model$module_collection$model, gets::as.lm)

  names(model_list) <- model$module_order$dependent

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

  table_output <- modelsummary::modelsummary(
    model_list,
    #coef_omit = "iis|sis|q_[0-9]+",
    coef_map = coef_order,
    gof_omit = "R",
    #output = "latex",
    title = "Final models run for each sub-module for the illustrative example of Austria.",
    notes = "Quarterly Dummies, Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
    stars = TRUE,
  )


  # this output can go straight into the latex
  table_change(table_output, label = "tab:regression_summary")


  # -- Diagnostics

  diagnostics_model(model) %>%
    rename(Module = module) %>%
    kable(format = "latex",booktabs = TRUE, digits = 3, label = "diagnostics", caption = "Diagnostic results for each sub-module.") %>%
    kable_styling()


  diagnostics_model(model) %>%
    mutate(across(c(AR,ARCH, `Super Exogeneity`), ~ round(.,4))) %>%
    DT::datatable() %>%
    DT::formatStyle(columns = c("AR", "ARCH", "Super Exogeneity"),
                    backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
    DT::formatRound(columns = c("AR", "ARCH", "Share of Indicators"),
                    digits = 4)

  model %>%
    network() -> p

  ggsave(p, width = 7, height = 5, file = paste0(fig_path,"EAERE_network_",country,".pdf"))
  ggsave(p, width = 7, height = 5, file = paste0(fig_path,"EAERE_network_",country,".png"))


  # -- Forecasting

  #f2 <- forecast_model(model, exog_fill_method = "AR")
  f2 <- forecast_model(model, exog_fill_method = forecast_method, plot = FALSE)

  # -- Forecasting Plotting

  plot(f2, first_date = "2015-01-01") +
    labs(title = paste0("Illustrative Example for ",case_when(country == "AT" ~ "Austria",
                                                              country == "FR" ~ "France",
                                                              country == "DE" ~ "Germany")), subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values.") +
    theme(text = element_text(family = "Myriad Pro"),
          plot.subtitle = element_markdown()) -> p

  ggsave(p, width = 7, height = 5, file = paste0(fig_path,"EAERE_forecast_simple_",country,".pdf"), device = cairo_pdf)
  ggsave(p, width = 7, height = 5, file = paste0(fig_path,"EAERE_forecast_simple_",country,".png"))



  plot(f2, exclude.exogenous = FALSE, first_date = "2015-01-01") +
    labs(title = paste0("Illustrative Example for ",case_when(country == "AT" ~ "Austria",
                                                              country == "FR" ~ "France",
                                                              country == "DE" ~ "Germany")),
         subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.") +
    theme(text = element_text(family = "Myriad Pro"),
          plot.subtitle = element_markdown()) -> p

  ggsave(p, width = 7, height = 5, file = paste0(fig_path,"EAERE_forecast_inclexog_",country,".pdf"), device = cairo_pdf)
  ggsave(p, width = 7, height = 5, file = paste0(fig_path,"EAERE_forecast_inclexog_",country,".png"))




  # --

  f2_insample <- forecast_insample(model, sample_share = .96, exog_fill_method = forecast_method)

  extract_dep_vars <- f2_insample$central %>% distinct(dep_var) %>% pull

  ggplot2::ggplot() +
    ggplot2::geom_line(data = model$full_data %>%
                         rename(dep_var = na_item) %>%
                         filter(dep_var %in% extract_dep_vars,
                                time > as.Date("2010-01-01")),
                       ggplot2::aes(x = .data$time, y = .data$values), linewidth = 1) +

    ggplot2::facet_wrap(~dep_var, scales = "free") +
    ggplot2::geom_ribbon(data = f2_insample$uncertainty, ggplot2::aes(ymin = .data$min, x = .data$time, ymax = .data$max, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = f2_insample$uncertainty, ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = f2_insample$uncertainty, ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +

    ggplot2::geom_line(data = f2_insample$central, ggplot2::aes(y = .data$value, x = .data$time, color = as.factor(.data$start)), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~.data$dep_var, scales = "free") +
    #ggplot2::scale_color_brewer(palette = "PRGn") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::coord_cartesian(expand = TRUE) +

    ggplot2::labs(x = NULL, y = NULL, title = "Automatic Forecasting Hindcasts") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   text = element_text(family = "Myriad Pro"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> p


  ggsave(p, width = 7, height = 5, file = paste0(fig_path,"EAERE_hindcast_",country,".pdf"), device = cairo_pdf)
}


# --- Scenario

f2 <- forecast_model(model, exog_fill_method = "auto", plot = FALSE)
exog_data_high <- f2$exog_data

exog_data_high %>%
  mutate(HICP_Electricity = 400) -> exog_data_high_ready

f2_high <- forecast_model(model, exog_predictions = exog_data_high_ready, plot = FALSE)

#
# plot(f2_high, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity") +
#   coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2025-01-01"))) +
#   labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.") +
#   theme(text = element_text(family = "Myriad Pro"),
#         plot.subtitle = element_markdown()) -> a
#
# plot(f2, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity") +
#   coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2025-01-01"))) +
#   labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.") +
#   theme(text = element_text(family = "Myriad Pro"),
#         plot.subtitle = element_markdown()) -> b


f2_high_data <- plot(f2_high, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity", return.data = TRUE)
f2_data <- plot(f2, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity", return.data = TRUE)


#viridis::viridis(4)
man_cols <- c(Observation = "#440154FF",
              `Endogenous Forecast` = "#31688EFF",
              `Exogenous Forecast` = "#35B779FF",
              `Insample Fit` = "#FDE725FF")


f2_data %>%
  mutate(name = "AR Forecast") %>%
  bind_rows(f2_high_data %>%
              mutate(name = NA) %>%
              mutate(name = case_when(na_item == "EmiCO2Combustion" & type == "Exogeneous Forecast" ~ "Scenario Price",
                                      na_item == "HICP_Electricity" ~ "Scenario Price",
                                      TRUE ~ name))) %>%

  ggplot(aes(x = time, y = values, linetype = name, color = type)) +
  geom_line() +
  coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2025-01-01"))) +
  facet_wrap(~na_item, scales = "free") +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +

  ggplot2::facet_wrap(~.data$na_item, scales = "free") +

  ggplot2::labs(x = NULL, y = NULL) +

  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  #ggplot2::scale_color_viridis_d() +
  ggplot2::scale_color_manual(values = man_cols) +

  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none",
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank()) +
  labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.<br>Dotted line is scenario run with HICP_Electricity = 400. Solid lines are an AR forecast.") +
  theme(text = element_text(family = "Myriad Pro"),
        plot.subtitle = element_markdown()) -> p


ggsave(p, width = 7, height = 5, file = fig_path,"EAERE_scenario.pdf", device = cairo_pdf)


f2_data %>%
  mutate(name = "AR Forecast") %>%
  bind_rows(f2_high_data %>%
              mutate(name = "Scenario Price")) %>%

  filter(na_item == "EmiCO2Combustion" & fit == "forecast") %>%

  summarise(emissions = sum(values, na.rm = TRUE), .by = name) %>%
  mutate(total = sum(emissions),
         diff = c(NA,diff(emissions)),
         rel_diff = diff/total)

# ----


# model_constrained <- run_model(specification = specification,
#                                dictionary = new_dict,
#                                inputdata_directory = fig_path,"",
#                                primary_source = "local",
#                                save_to_disk = "data-raw/test_for_now/EAERE_data.xlsx",
#
#                                present = FALSE,
#                                quiet = FALSE,
#                                constrain.to.minimum.sample = TRUE)
#
# f2_cons <- forecast_model(model_constrained, exog_fill_method = "auto")
#
# f2_cons_insample <- forecast_insample(model_constrained, sample_share = .9, exog_fill_method = "auto")
#
# f2_cons_insample$plot + geom_hline(aes(yintercept = 0))

