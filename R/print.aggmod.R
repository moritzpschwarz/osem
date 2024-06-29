#' Print output of an Aggregate Model
#'
#' @param x An object of type 'aggmod'
#' @param ... Further arguments.
#'
#' @export
#'

print.aggmod <- function(x, ...){


  cat("Aggregate Model Output\n")
  cat("-----------------------\n")

  cat(paste0("\nEstimation Options:"))
  cat(paste0("\nSample: ", min(x$full_data$time)," to ",max(x$full_data$time)))
  cat(paste0("\nMax AR Considered:"), ifelse(is.null(x$args$max.ar), 4, x$args$max.ar))
  cat(paste0("\nEstimation Option:"), ifelse(is.null(x$args$ardl_or_ecm), "ARDL", x$args$ardl_or_ecm))
  cat("\n")



  cat("\nRelationships considered: \n")

  dictionary <- {if(is.null(x$args$dictionary)){
    aggregate.model::dict
  } else {
    x$args$dictionary
  } } %>%
    dplyr::select("model_varname", "full_name") %>%
    dplyr::mutate(dependent = .data$model_varname,
                  splitvars = .data$model_varname)



  x$module_order %>%
    dplyr::select("index", "order", "dependent", "independent") %>%

    # deal with dependent vars
    dplyr::left_join(dictionary %>%
                       dplyr::select("dependent", "full_name"), by = "dependent") %>%
    dplyr::relocate("full_name", .after = "dependent") %>%

    # deal with independet vars
    dplyr::mutate(ind_spaced = .data$independent,
                  independent = gsub(" ", "", .data$independent)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(splitvars = list(strsplits(.data$independent,c("\\-", "\\+")))) %>%
    tidyr::unnest("splitvars", keep_empty = TRUE) %>%
    dplyr::left_join(dictionary %>%
                       dplyr::select("splitvars", "full_name") %>%
                       dplyr::rename(name_ind = "full_name"), by = "splitvars") %>%

    dplyr::group_by(.data$index, .data$dependent, .data$full_name, .data$ind_spaced) %>%
    dplyr::summarise(ind_name = toString(.data$name_ind), .groups = "drop") %>%
    dplyr::mutate(ind_name = ifelse(.data$ind_name == "NA","Only AR Specification", .data$ind_name)) %>%

    # styling
    dplyr::rename(`Ind. Var` = "ind_spaced",
                  `Model` = "index",
                  #`Est. Order` = order,
                  `Dep. Var.` = "dependent",
                  `Full Name Ind. Var` = "ind_name",
                  `Full Name Dep. Var` = "full_name") %>%

    print


  cat("\n\nRelationships estimated in the order: ",paste0(x$module_collection$index, collapse = ","))


  stars.pval <- function(x) {
    stars <- c("***", "**", "*", "")
    var <- c(0, 0.01, 0.05, 0.10, 1)
    i <- findInterval(x, var, left.open = TRUE, rightmost.closed = TRUE)
    stars[i]
  }

  format.pval <- function(x, digits = 3) {
    if (is.na(x)) {
      return("")
    }
    if (x < 0.001) {
      return("<0.001***")
    }
    if (x > 1) {
      return(">0.999")
    }
    return(paste0(formatC(x, format = "f", digits = digits), stars.pval(x)))
  }


  cat("\n\nDiagnostics:\n ")
  print(diagnostics_model(x) %>%
          dplyr::rename(`Dependent Variable` = "module") %>%
          dplyr::mutate(dplyr::across(c("AR","ARCH","Super Exogeneity"), ~paste0(format.pval(.)))))

}
