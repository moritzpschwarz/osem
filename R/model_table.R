#' Show the final regression equations estimated by the OSEM Model
#'
#' @param model An OSEM model object or class \code{osem}, created by the \code{run_model} function.
#' @param output Character. Either "default", "html", "data.frame", or "latex". Default is "default" (which is the default in the \code{\link[modelsummary]{modelsummary}} function.
#' @param title Character. The title of the table. Default is "OSEM Model Results".
#'
#' @return Either an html or a text (latex) table with the regression results.
#' @export
#'
model_table <- function(model,
                        output = "default",
                        title = "OSEM Model Results"){

  model_list <- lapply(model$module_collection$model, gets::as.lm)

  names(model_list) <- model$module_order$dependent



  # ensure the correct order of the table
  lapply(model_list, broom::tidy) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(.data$term) %>%
    dplyr::filter(!stringr::str_detect(.data$term, "iis|sis|q_[0-9]+")) %>%
    dplyr::mutate(base = gsub("L[0-9]+\\.","",.data$term)) %>%
    dplyr::mutate(base_num = dplyr::case_when(.data$base == "mconst" ~ 2,
                                              .data$base == "trend" ~ 3,
                                              grepl("ar[0-9]+",.data$base) ~ 1,
                                              TRUE ~ 4)) %>%
    dplyr::arrange(.data$base_num, .data$base, dplyr::desc(.data$term)) %>%
    dplyr::pull("term") -> coef_order

  table_output <- modelsummary::modelsummary(
    model_list,
    coef_map = coef_order,
    gof_omit = "R",
    output = output,
    title = title,
    notes = "Quarterly Dummies, Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
    stars = TRUE,
  )

  # For LATEX
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

  # this output can go straight into the latex
  if(output == "latex"){return(table_change(table_output, label = "tab:regression_summary"))}

  if(output %in% c("html","data.frame", "default")){return(table_output)}

}



