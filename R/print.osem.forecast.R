#' Print output of an OSEM Forecast
#'
#' @param x An object of type 'osem.forecast'
#' @param plot Logical. Default = \code{TRUE}. Should the osem forecast output be plotted?
#' @param full_names Logical. Default = \code{FALSE}. Should the full names of the variables from the dictionary be printed?
#' @param ... Further arguments.
#'
#' @return A printed summary of the OSEM forecast output, including the forecast horizon, method, and central estimates.
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

  # get log information
  if(!is.null(x$orig_model$opts_df[["log_opts"]])){
    x$orig_model$opts_df %>%
      dplyr::mutate(log_opts_dependent = purrr::map2(.data$log_opts, .data$dependent, function(opts,dep){
        opts[,dep, drop = TRUE]
      })) %>%
      tidyr::unnest("log_opts_dependent", keep_empty = TRUE) %>%
      tidyr::replace_na(list(log_opts_dependent = "none")) %>%
      dplyr::select(c("dep_var" = "dependent","log_opt" = "log_opts_dependent")) -> log_opts_processed
  } else {
    log_opts_processed <- dplyr::tibble(dep_var = x$orig_model$opts_df$dependent, log_opt = "none")
  }

  x$forecast %>%
    dplyr::select("dep_var", "central.estimate") %>%
    tidyr::unnest("central.estimate") %>%
    tidyr::pivot_longer(-c("time","dep_var")) %>%
    tidyr::drop_na() %>%
    dplyr::full_join(log_opts_processed, by = "dep_var") %>%
    dplyr::mutate(value = dplyr::case_when(.data$log_opt == "log" ~ exp(.data$value),
                                           .data$log_opt == "asinh" ~ sinh(.data$value),
                                           .data$log_opt == "none" ~ .data$value)) %>%
    dplyr::select(-c("name", "log_opt")) %>%
    tidyr::pivot_wider(id_cols = "time", names_from = "dep_var") %>%
    dplyr::rename("Date" = "time") %>%
    dplyr::arrange(.data$Date) -> fcast_table


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
