#' Translate model variable names to Eurostat codes
#'
#' Takes a specification and translates the model variable names into Eurostat
#' codes. Also adds the Eurostat dataset identifier from which the variable is
#' extracted.
#'
#' @param specification A tibble or data.frame with one column named 'dependent'
#'   that contains the LHS (Y) variables and one column named 'independent' that
#'   contains the RHS (x) variables, separated by + and - . Column 'type' should
#'   specify whether the dependent variable is endogenous by modelling
#'   (\code{"n"}) or endogenous by accounting definition (\code{"d"}).
#' @param dictionary A tibble or data.frame storing the model variable names in
#'   a column named 'model_varname' and the corresponding Eurostat variable code
#'   in 'eurostat_code'. When \code{NULL}, the \link[=dict]{default dictionary}
#'   is used.
#'
#' @return Returns a tibble or data.frame as the input \code{specification} with
#'   two added columns for the model variable name (\code{model_varname}) and
#'   the dataset identifier (\code{dataset_id}).
#'
#' @export


translate_variables <- function(specification, dictionary = NULL) {

  if (is.null(dictionary)) {
    dictionary <- aggregate.model::dict
  }

  # vectors of dependent and independent parts
  dep <- specification$dependent
  indep <- gsub(" ", "", specification$independent)

  # extract all separators "-" or "+" for all RHS formulas
  seps <- str_extract_all(string = indep, pattern = "\\-|\\+")

  # replace dependent variable names by Eurostat code
  for (i in seq_along(dep)) {
    ind <- which(dictionary$model_varname == dep[i])
    if (length(ind) == 1L) {
      dep[i] <- dictionary$eurostat_code[ind]
    }
  }

  # replace independent variable names
  for (i in seq_along(indep)) {

    # each element of character vector may contain formula, e.g. "A + B", must split
    vars <- strsplits(
      indep[i],
      c("\\-", "\\+"))


    # go through elements of formula and replace variable names by Eurostat code
    for (j in seq_along(vars)) {
      ind <- which(dictionary$model_varname %in% vars[j])
      if (length(ind) == 1L) {
        vars[j] <- dictionary$eurostat_code[ind]
      }
    }

    # stitch back together using correct separators
    res <- character(0)
    for (k in seq_along(vars)) {
      res <- paste(res, vars[k], sep = "")
      if (k <= length(seps[[i]])) {
        res <- paste(res, seps[[i]][k], sep = "")
      }
    }

    res <- gsub("\\+", " + ", res)
    res <- gsub("\\-", " - ", res)

    if(identical(res,character(0))){res <- ""}

    # replace result
    indep[i] <- res

  }

  # add the translated columns
  specification <- specification %>%
    mutate(dependent_eu = dep,
           independent_eu = indep) %>%
    relocate(dependent_eu, .after = dependent) %>%
    relocate(independent_eu, .after = independent)

  return(specification)


  # Moritz' original comments, check whether fulfilled or not
  # take in variables either from NAM or elsewhere

  # translate variables to eurostat codes

  # return dataframe with both with the same codes (order, dependent, independent, etc.)

}
