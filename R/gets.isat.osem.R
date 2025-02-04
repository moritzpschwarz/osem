gets.isat.osem <- function (x, t.pval = 0.05, wald.pval = t.pval, vcov.type = NULL,
                            do.pet = TRUE, ar.LjungB = list(lag = NULL, pval = 0.025),
                            arch.LjungB = list(lag = NULL, pval = 0.025), normality.JarqueB = NULL,
                            user.diagnostics = NULL, info.method = c("sc", "aic", "aicc",
                                                                     "hq"), gof.function = NULL, gof.method = NULL, keep = NULL,
                            include.gum = FALSE, include.1cut = TRUE, include.empty = FALSE,
                            max.paths = NULL, tol = 1e-07, turbo = FALSE, print.searchinfo = TRUE,
                            plot = NULL, alarm = FALSE, ...)
{
  if (missing(vcov.type)) {
    vcov.type <- x$aux[["vcov.type"]]
  }
  if (missing(user.diagnostics)) {
    user.diagnostics <- x$aux[["user.diagnostics"]]
  }
  if (missing(tol)) {
    tol <- x$aux$tol
  }
  if (missing(normality.JarqueB)) {
    if (!is.null(x$call$normality.JarqueB)) {
      normality.JarqueB <- x$call$normality.JarqueB
    }
  }
  if (missing(arch.LjungB)) {
    arch.LjungB <- x$call$arch.LjungB
  }
  if (missing(ar.LjungB)) {
    ar.LjungB <- x$call$ar.LjungB
  }
  user.estimator <- x$aux$user.estimator
  LAPACK <- x$aux$LAPACK
  y <- x$aux$y
  y <- as.matrix(y)
  colnames(y) <- x$aux$y.name
  mxreg <- x$aux$mX
  colnames(mxreg) <- x$aux$mXnames
  tmpmc <- getOption("mc.warning")
  options(mc.warning = FALSE)

  # reformulate the do.call below into a simple gets::arx
  object <- gets::arx(y = y, mxreg = mxreg, ewma = NULL, mc = FALSE, ar = NULL, log.ewma = NULL,
                      vc = FALSE, arch = NULL, asym = NULL, vxreg = NULL, zero.adj = 0.1,
                      vc.adj = TRUE, qstat.options = NULL, vcov.type = vcov.type,
                      normality.JarqueB = normality.JarqueB, user.estimator = user.estimator,
                      user.diagnostics = user.diagnostics, tol = tol, LAPACK = LAPACK,
                      singular.ok = TRUE, plot = NULL)

  object$call$user.estimator <- user.estimator
  object$call$user.diagnostics <- user.diagnostics

  out <- gets::getsm(object = object, t.pval = t.pval, wald.pval = wald.pval, vcov.type = vcov.type, do.pet = do.pet,
                     ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB, normality.JarqueB = normality.JarqueB, user.diagnostics = user.diagnostics,
                     info.method = info.method, gof.function = gof.function, gof.method = gof.method, keep = keep, include.gum = include.gum,
                     include.1cut = include.1cut, include.empty = include.empty, max.paths = max.paths, tol = tol, turbo = turbo, print.searchinfo = print.searchinfo,
                     plot = plot, alarm = alarm)

  out$call[1] <- "getsm"
  options(mc.warning = tmpmc)
  return(out)
}
