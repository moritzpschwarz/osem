#' OSEM adaptation of as.arx.isat()
#'
#' @param object An \code{isat} object
#' @param ... Further arguments
#'
#' @returns An \code{arx} object

as.arx.isat.osem <- function(object, ...){
  ## check class
  if (!is(object, "isat")) {
    objectName <- deparse(substitute(object))
    stop(paste0("'", objectName, "' not of class 'isat'"))
  }

  dots <- list(...)
  isat.args <- object$aux[["args"]]

  get_arg <- function(name, default = NULL) {
    if (name %in% names(dots)) {return(dots[[name]])}
    if (!is.null(isat.args) && name %in% names(isat.args)) {
      return(isat.args[[name]])
    }
    default
  }

  vcov.type <- get_arg("vcov.type", object$aux[["vcov.type"]])
  user.diagnostics <- get_arg("user.diagnostics", object$aux[["user.diagnostics"]])
  tol <- get_arg("tol", object$aux[["tol"]])
  normality.JarqueB <- get_arg("normality.JarqueB", FALSE)
  if (is.null(normality.JarqueB)) normality.JarqueB <- FALSE
  ar <- get_arg("ar", NULL)

  arch.LjungB <- get_arg("arch.LjungB", NULL)
  ar.LjungB <- get_arg("ar.LjungB", NULL)
  user.estimator <- get_arg("user.estimator", object$aux[["user.estimator"]])
  LAPACK <- get_arg("LAPACK", object$aux[["LAPACK"]])

  consumed <- c(
    "vcov.type",
    "user.diagnostics",
    "tol",
    "normality.JarqueB",
    "arch.LjungB",
    "ar.LjungB",
    "user.estimator",
    "LAPACK",
    "ar"
  )

  dots_for_arx <- dots[setdiff(names(dots), consumed)]

  ## Reconstruct y with original index
  yName <- object$aux[["y.name"]]
  y <- zoo::zoo(x = object$aux[["y"]], order.by = object$aux[["y.index"]])

  ## Reconstruct mxreg with original index
  mxreg <- object$aux[["mX"]]
  if (!is.null(mxreg)) {colnames(mxreg) <- object$aux[["mXnames"]]}

  ## Constant handling
  has_mconst <- !is.null(mxreg) && "mconst" %in% colnames(mxreg)
  if (has_mconst) {mxreg <- mxreg[, colnames(mxreg) != "mconst", drop = FALSE]}
  mc <- has_mconst

  # ar handling
  ar_original <- ar
  ar_for_arx <- NULL
  # if (!is.null(ar) && length(ar) > 0 && !is.null(mxreg)) {
  #   ar_names <- paste0("ar", ar)
  #   mxreg <- mxreg[,!colnames(mxreg) %in% ar_names,drop = FALSE]
  # }

  if (!is.null(mxreg) && NCOL(mxreg) == 0) {mxreg <- NULL}
  if (!is.null(mxreg)) {mxreg <- zoo::zoo(x = mxreg, order.by = object$aux[["y.index"]])}

  ## Estimate arx object
  arx_args <- c(
    list(
      y = y,
      mxreg = mxreg,
      mc = mc,
      ewma = NULL,
      ar = ar_for_arx,
      log.ewma = NULL,
      vc = FALSE,
      arch = NULL,
      asym = NULL,
      vxreg = NULL,
      zero.adj = 0.1,
      vc.adj = TRUE,
      qstat.options = NULL,
      vcov.type = vcov.type,
      normality.JarqueB = normality.JarqueB,
      #arch.LjungB = arch.LjungB,
      #ar.LjungB = ar.LjungB,
      user.estimator = user.estimator,
      user.diagnostics = user.diagnostics,
      tol = tol,
      LAPACK = LAPACK,
      singular.ok = TRUE,
      plot = NULL
    ),
    dots_for_arx
  )

  result <- do.call(gets::arx, arx_args)

  ## Patch metadata / evaluated call values
  result$aux$y.name <- yName
  result$call$mc <- mc
  result$call$vcov.type <- vcov.type
  result$call$normality.JarqueB <- normality.JarqueB
  result$call$user.estimator <- user.estimator
  result$call$user.diagnostics <- user.diagnostics
  result$call$tol <- tol
  result$call$LAPACK <- LAPACK

  result$call$ar <- ar
  if(!is.null(ar)){
    diag_result <- result
    diag_result$std.residuals <- as.numeric(diag_result$std.residuals)
    #row.names(result$diagnostics) <- gsub("AR\\(1\\)",paste0("AR(",max(ar),")"),row.names(result$diagnostics))
    result$diagnostics <- gets::diagnostics(diag_result, ar.LjungB = c(max(ar)+1,0))
  }

  return(result)
}
