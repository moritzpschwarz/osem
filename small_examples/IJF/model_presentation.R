library(tidyverse)
library(kableExtra)


for(country in c("DE","AT","FR", "DK")){

  # country = "DE"

  # to do:
  # add equation from Felix --> DONE
  # add environmental equations --> DONE
  # transfer to other countries --> DONE
  # change to EDGAR v9 --> DONE
  # change environmental variables to real values --> DONE
  # fix Flights and Inflation


  load(paste0("./small_examples/IJF/", country, "/model.RData"))

  plot(model_result_ext)

  plot(model_result_ext, grepl_variables = "EmiCO2")

  fc_ext <- forecast_model(model_result_ext, exog_fill_method = "auto")


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

  table_output

  # this output can go straight into the latex
  table_change(table_output, label = "tab:regression_summary")


  # -- Diagnostics

  diagnostics_model(model_result_ext) %>%
    rename(Module = module) %>%
    #kable(format = "latex",booktabs = TRUE, digits = 3, label = "diagnostics", caption = "Diagnostic results for each sub-module.") %>%
    kable(booktabs = TRUE, digits = 3, label = "diagnostics", caption = "Diagnostic results for each sub-module.") %>%
    kable_styling()


  diagnostics_model(model_result_ext) %>%
    mutate(across(c(AR,ARCH, `Super Exogeneity`), ~ round(.,4))) %>%
    DT::datatable() %>%
    DT::formatStyle(columns = c("AR", "ARCH", "Super Exogeneity"),
                    backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
    DT::formatRound(columns = c("AR", "ARCH", "Share of Indicators"),
                    digits = 4)




  (model_result_ext %>%
    network() -> p)

  # ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/Network",".pdf"))
  # ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/Network",".png"), bg = "white")



  insample <- forecast_insample(model_result_ext, sample_share = .96, exog_fill_method = c("AR","auto"))


}
