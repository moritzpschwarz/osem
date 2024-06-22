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


translate_variables <- function(specification, dictionary = NULL) {

  if (is.null(dictionary)) {
    dictionary <- aggregate.model::dict
  }

  # vectors of dependent and independent parts
  dep <- specification$dependent
  indep <- gsub(" ", "", specification$independent)

  # extract all separators "-" or "+" for all RHS formulas
  seps <- stringr::str_extract_all(string = indep, pattern = "\\-|\\+")

  # replace dependent variable names by variable code
  for (i in seq_along(dep)) {
    if(all(!dictionary$model_varname %in% dep[i])){stop(paste0("Model Variable Name '",dep[i],"' not found in dictionary. Check both dictionary and specification."))}
    ind <- which(dictionary$model_varname == dep[i]) # gets the row of the dictionary
    if (length(ind) == 1L) {
      dep[i] <- dictionary$variable_code[ind]
      # add the NACE Sector to the eurostat code
      if (!is.na(dictionary$nace_r2[ind])) {dep[i] <- paste0(dep[i],"*",dictionary$nace_r2[ind])}
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
      if(all(!dictionary$model_varname %in% vars[j])){stop(paste0("Model Variable Name '",vars[j],"' not found in dictionary. Check both dictionary and specification."))}
      ind <- which(dictionary$model_varname %in% vars[j])
      if (length(ind) == 1L) {
        vars[j] <- dictionary$variable_code[ind]
        # add the NACE Sector to the eurostat code
        if (!is.na(dictionary$nace_r2[ind])) {vars[j] <- paste0(vars[j],"*",dictionary$nace_r2[ind])}
      }
    }

    # stitch back together using correct separators
    res <- character(0)
    for (k in seq_along(vars)) {
      res <- paste(res, vars[k], sep = "")
      if (k <= length(seps[[i]])) {
        res <- paste(res, " ", seps[[i]][k], " ", sep = "")
      }
    }

    # res <- gsub("\\+", " + ", res)
    # res <- gsub("\\-", " - ", res)

    if(identical(res,character(0))){res <- ""}

    # replace result
    indep[i] <- res

  }

  # add the translated columns
  specification <- specification %>%
    dplyr::mutate(dependent_code = dep,
                  independent_code = indep) %>%
    dplyr::relocate("dependent_code", .after = "dependent") %>%
    dplyr::relocate("independent_code", .after = "independent")

  return(specification)


  # Moritz' original comments, check whether fulfilled or not
  # take in variables either from NAM or elsewhere

  # translate variables to eurostat codes

  # return dataframe with both with the same codes (order, dependent, independent, etc.)

}
