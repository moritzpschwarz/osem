#' Creates a Summary of the Diagnostics of the Aggregate Model
#'
#' @param model An aggregate model of class 'osem'
#'
#' @return Returns a data.frame with the p-values of the AR and ARCH
#'   misspecification tests and the number of impulse and step indicators
#'   retained for each module.
#'
#' @export

diagnostics_model <- function(model) {

  models <- model$module_collection$model
  # list of length = number of modules; NULL if is identity

  # give names to the elements in the list (b/c internally might always say dep var = "y")
  names(models) <- model$module_collection$dependent

  # get rid of NULL elements
  models <- models[!sapply(models,is.null)]

  # diagnostics
  diag <- data.frame(module = names(models),
                     AR = NA,
                     ARCH = NA,
                     IIS = NA,
                     SIS = NA,
                     n = NA,
                     indicator_share = NA)

  # fill in the values
  for (i in 1:length(models)) {

    # extract module and check that it is an isat object
    module <- models[[i]]
    stopifnot(inherits(module, "isat"))

    # record diagnostics
    d <- module$diagnostics
    # usually, first entry is AR and second is ARCH but could be different (e.g. when add other tests or not OLS)
    # so be a bit more careful how to select the columns
    ar_where <- grepl(pattern = "^Ljung-Box AR\\(", x = rownames(d))
    arch_where <- grepl(pattern = "^Ljung-Box ARCH\\(", x = rownames(d))
    # sanity check that were uniquely identified
    stopifnot(sum(ar_where) == 1)
    stopifnot(sum(arch_where) == 1)
    # populate with p-values
    diag[i, "AR"] <- d[ar_where, "p-value"]
    diag[i, "ARCH"] <- d[arch_where, "p-value"]

    # records indicators
    if (length(module$ISnames) == 0L) {
      diag[i, "IIS"] <- 0
      diag[i, "SIS"] <- 0
    } else {
      indicators <- module$ISnames
      iis <- indicators[grepl(pattern = "^iis[[:digit:]]+", x = indicators)]
      sis <- indicators[grepl(pattern = "^sis[[:digit:]]+", x = indicators)]
      # if not present, returns empty character
      diag[i, "IIS"] <- length(iis)
      diag[i, "SIS"] <- length(sis)
    }

    # record number of observations
    diag[i, "n"] <- module$n

    # record share of indicators retained
    diag[i, "indicator_share"] <- (diag[i, "IIS"] + diag[i, "SIS"]) / diag[i, "n"]

  } # end for

  return(diag)

}
