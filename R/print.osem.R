#' Print output of an OSEM Model
#'
#' @param x An object of type 'osem'
#' @param plot Logical. Default = \code{TRUE}. Should the osem model output be plotted?
#' @param full_names Logical. Default = \code{FALSE}. Should the full names of the variables from the dictionary be printed?
#' @param ... Further arguments.
#'
#' @return A printed summary of the OSEM model output, including estimation options, relationships considered, and diagnostics.
#'
#' @export

print.osem <- function(x, plot = TRUE, full_names = FALSE, ...){


  cat("OSEM Model Output\n")
  cat("-----------------------\n")

  cat(paste0("\nEstimation Options:"))
  cat(paste0("\nSample: ", min(x$full_data$time)," to ",max(x$full_data$time)))
  cat(paste0("\nMax AR Considered:"), ifelse(is.null(x$args$max.ar), 4, x$args$max.ar))
  cat(paste0("\nEstimation Option:"), ifelse(is.null(x$args$ardl_or_ecm), "ARDL", x$args$ardl_or_ecm))
  cat("\n")



  cat("\nRelationships considered: \n")

  dictionary <- {if(is.null(x$args$dictionary)){
    osem::dict
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

    {if(!full_names){dplyr::select(.,-c("Full Name Ind. Var","Full Name Dep. Var"))} else {.}} -> rel_styled

  cat(format(rel_styled)[-3L], sep = "\n")

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

  diag <- diagnostics_model(x)
  if(nrow(diag) > 0){
    cat("\n\nDiagnostics:\n ")
    print(diagnostics_model(x) %>%
            dplyr::rename(`Dependent Variable` = "module") %>%
            dplyr::rowwise() %>%
            dplyr::mutate(dplyr::across(c("AR","ARCH","Super Exogeneity"), ~paste0(format.pval(.)))) %>%
            dplyr::ungroup())
  } else {
    cat("\n\nDiagnostics:\nNo Diagnostics available.\n")
  }

  if(plot){
    plot(x)
  }

}
