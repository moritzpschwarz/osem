#' OSEM Diagnostics
#'
#' Creates a Summary of the Diagnostics of the OSEM Model
#'
#' @param model A model of class 'osem' (to be returned by \code{\link{run_model}}).
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

  # now do the same for super exogeneity tests
  other.test.objects <- model$module_collection$diagnostics
  names(other.test.objects) <- model$module_collection$dependent

  # get rid of NULL elements
  models <- models[!sapply(models,is.null)]

  # diagnostics
  diag <- dplyr::tibble(module = names(models),
                        AR = NA,
                        ARCH = NA,
                        `Super Exogeneity` = NA,
                        IIS = NA,
                        SIS = NA,
                        n = NA,
                        `Share of Indicators` = NA)

  # fill in the values
  for (i in seq_along(models)) {

    # extract module and check that it is an isat object
    module <- models[[i]]

    if(inherits(module, "isat")){
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
      diag[i, "Share of Indicators"] <- (diag[i, "IIS"] + diag[i, "SIS"]) / diag[i, "n"]

      # record other test objects (currently super.exogeneity, in the future cointegration)
      super.ex_obj <- other.test.objects[[names(models[i])]][["super.exogeneity"]]

      if(is.list(super.ex_obj)){
        diag[i, "Super Exogeneity"] <- super.ex_obj$p.value
      } else {
        diag[i, "Super Exogeneity"] <- NA
      }

    }

    if (inherits(module, "osem.cvar")) {
      resid_mat <- module$varm$resid

      # AR test
      ar_pval <- tryCatch({
        mh <- mahalanobis(resid_mat, center = colMeans(resid_mat), cov = cov(resid_mat))
        Box.test(mh, lag = 12, type = "Ljung-Box")$p.value
      }, error = function(e) NA)

      # ARCH test (Engle)
      arch_pval <- tryCatch({
        u2 <- rowSums(resid_mat^2)
        aux_df <- embed(u2, 13)
        y <- aux_df[, 1]
        X <- aux_df[, -1]
        fit <- lm(y ~ X)
        R2 <- summary(fit)$r.squared
        n <- nrow(X)
        stat <- R2 * n
        pchisq(stat, df = ncol(X), lower.tail = FALSE)
      }, error = function(e) NA)

      diag[i, "AR"]   <- ar_pval
      diag[i, "ARCH"] <- arch_pval
      diag[i, "IIS"]  <- NA
      diag[i, "SIS"]  <- NA
      diag[i, "Share of Indicators"] <- NA

      diag[i, "n"] <- module$varm$obs
      diag[i, "K"] <- module$varm$K
      diag[i, "Rank"] <- module$rank

      # Trace test diagnostic
      trace_stats <- module$cointtest@teststat
      trace_crit  <- module$cointtest@cval
      r <- module$rank
      diag[i, "Trace Stat (r)"] <- trace_stats[r]
      diag[i, "Trace 5pct Crit"] <- trace_crit[r, "5pct"]
      diag[i, "Trace Test Passed"] <- trace_stats[r] > trace_crit[r, "5pct"]

      # TODO: report further diagnostics
      # Unit root test for Y variable(s)

      # Super exogeneity not applicable
      diag[i, "Super Exogeneity"] <- NA
    }


  } # end for

  return(diag)

}
