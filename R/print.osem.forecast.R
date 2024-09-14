#' Print output of an OSEM Forecast
#'
#' @param x An object of type 'osem.forecast'
#' @param plot Logical. Default = \code{TRUE}. Should the osem forecast output be plotted?
#' @param full_names Logical. Default = \code{FALSE}. Should the full names of the variables from the dictionary be printed?
#' @param ... Further arguments.
#'
#' @export

print.osem.forecast <- function(x, plot = TRUE, full_names = FALSE, ...){


  cat("OSEM Model Forecast Output\n")
  cat("-----------------------\n")

  cat(paste0("\nForecast Overview:"))

  forecast_times <- dplyr::bind_rows(x$forecast$central.estimate)$time

  cat(paste0("\nForecast Horizon: ", min(forecast_times)," to ",max(forecast_times)))
  cat(paste0("\nForecast Method:"), dplyr::case_when(x$args$exog_fill_method == "AR" ~ "AR Model (Outlier and Step Shift Corrected)",
                                                     x$args$exog_fill_method == "auto" ~ "AR Model run with auto.arima()",
                                                     x$args$exog_fill_method == "last" ~ "Last Observed Value",
                                                     TRUE ~ x$args$exog_fill_method))
  if(x$args$exog_fill_method %in% c("AR")){cat(paste0("\nMax AR Length:"), x$args$ar.fill.max)}
  cat("\n")


  cat("\nCentral Forecast Estimates: \n")

  dplyr::bind_rows(x$forecast$central.estimate) %>%
    tidyr::pivot_longer(-"time") %>%
    tidyr::drop_na("value") %>%
    tidyr::pivot_wider(id_cols = "time") %>%
    dplyr::rename("Date" = "time") -> fcast_table


  cat(format(fcast_table)[-3L], sep = "\n")


  if(plot){
    print(plot(x))
  }

  #
  # stars.pval <- function(x) {
  #   stars <- c("***", "**", "*", "")
  #   var <- c(0, 0.01, 0.05, 0.10, 1)
  #   i <- findInterval(x, var, left.open = TRUE, rightmost.closed = TRUE)
  #   stars[i]
  # }
  #
  # format.pval <- function(x, digits = 3) {
  #   if (is.na(x)) {
  #     return("")
  #   }
  #   if (x < 0.001) {
  #     return("<0.001***")
  #   }
  #   if (x > 1) {
  #     return(">0.999")
  #   }
  #   return(paste0(formatC(x, format = "f", digits = digits), stars.pval(x)))
  # }
  #
  # cat("\n\nDiagnostics:\n ")
  # print(diagnostics_model(x) %>%
  #         dplyr::rename(`Dependent Variable` = "module") %>%
  #         dplyr::rowwise() %>%
  #         dplyr::mutate(dplyr::across(c("AR","ARCH","Super Exogeneity"), ~paste0(format.pval(.)))) %>%
  #         dplyr::ungroup())
  #
  #

}
