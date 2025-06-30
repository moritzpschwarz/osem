#' Print output of an OSEM Insample Forecast
#'
#' @param x An object of class osem.forecast.insample, which is the output from the \link{forecast_insample} function.
#' @param plot Logical. Default = \code{TRUE}. Should the osem insample forecast output be plotted?
#' @param full_names Logical. Default = \code{FALSE}. Should the full names of the variables from the dictionary be printed?
#' @param ... Further arguments.
#'
#' @return A printed summary of the insample forecast output, including RMSFE and forecast within uncertainty statistics.
#'
#' @export

print.osem.forecast.insample <- function(x, plot = TRUE, full_names = FALSE, ...){

  if(!isa(x, "osem.forecast.insample")){
    stop("Input object not of type 'osem.forecast.insample'. Run 'forecast_insample' again and use the output of that function.")
  }

  cat("OSEM Insample Model Forecast Output\n")
  cat("-----------------------\n")

  cat(paste0("\nInsample Forecast Overview:"))
  cat(paste0("\nForecast Sample Share: ", x$args$sample_share * 100, "%"))
  cat(paste0("\nForecast Horizon: ", min(x$args$time_to_use)," to ",max(x$args$time_to_use)))

  fill_method <- dplyr::case_when(x$args$exog_fill_method == "AR" ~ "AR Model (Outlier and Step Shift Corrected)",
                                  x$args$exog_fill_method == "auto" ~ "AR Model run with auto.arima()",
                                  x$args$exog_fill_method == "last" ~ "Last Observed Value",
                                  TRUE ~ x$args$exog_fill_method)
  # if fill_method is longer than length 1, then list them with commas and & at the end
  if(length(fill_method) > 1){
    fill_method <- paste0(paste0(fill_method[1:(length(fill_method)-1)], collapse = ", "), " & ", fill_method[length(fill_method)])
  }

  cat(paste0("\nForecast Method:"), fill_method)
  cat("\n")

  cat("-----------------------\n")
  cat("\nRMSFE: \n")
  x$rmsfe %>%
    tidyr::pivot_wider(id_cols = c("start","method"),
                       names_from = "na_item",
                       values_from = "rmsfe") %>%
    dplyr::relocate("Total RMSFE", .after = "method") %>%
    dplyr::rename(Start = "start",
                  Method = "method") -> rmsfe_table

  cat(format(rmsfe_table)[-c(1L,3L)], sep = "\n")

  cat("-----------------------\n")
  cat("\nForecast Within Uncertainty: \n")

  if(is.null(x$forecast_failures[["na_item"]])){
    cat("No forecast failures estimated (potentially due to entire model being an identity).\n")
  } else {

    x$forecast_failures %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("failure"),
                                  .fns = ~ dplyr::if_else(. == "Success", 1, 0))) %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("failure"), ~sum(.)/dplyr::n()), .by = c("method","na_item")) %>%
      dplyr::rename_with(.fn = ~gsub("failure","success",.)) %>%
      tidyr::pivot_longer(-c("na_item","method")) %>%
      tidyr::pivot_wider(id_cols = c("method","name"), names_from = "na_item", values_from = "value") %>%
      dplyr::mutate(name = dplyr::case_when(name == "success_all" ~ "Forecast within Uncertainty",
                                            name == "success_95" ~ "Forecast within 95% Uncertainty",
                                            name == "success_90" ~ "Forecast within 90% Uncertainty",
                                            name == "success_50" ~ "Forecast within IQR"),
                    dplyr::across(-c("name","method"), scales::percent)) %>%
      dplyr::mutate(name = factor(.data$name,levels = c("Forecast within Uncertainty",
                                                        "Forecast within 95% Uncertainty",
                                                        "Forecast within 90% Uncertainty",
                                                        "Forecast within IQR"))) %>%
      dplyr::arrange(.data$name) %>%
      dplyr::rename(Measure = "name",
                    Method = "method")  -> forecast_failures_tab

    cat(format(forecast_failures_tab)[-c(1L,3L)], sep = "\n")

  }

  try(suppressMessages(if(plot){
    print(plot(x))
  }), silent = TRUE)


}
