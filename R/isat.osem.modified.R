isat.osem.modified <- function (y, mc = TRUE, ar = NULL, ewma = NULL, mxreg = NULL,
                                iis = FALSE, sis = TRUE, tis = FALSE, uis = FALSE, blocks = NULL,
                                ratio.threshold = 0.8, max.block.size = 30, t.pval = 0.001,
                                wald.pval = t.pval, vcov.type = c("ordinary", "white", "newey-west"),
                                do.pet = FALSE, ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = NULL,
                                info.method = c("sc", "aic", "hq"), user.diagnostics = NULL,
                                user.estimator = NULL, gof.function = NULL, gof.method = c("min",
                                                                                           "max"), include.gum = TRUE, include.1cut = FALSE, include.empty = FALSE,
                                max.paths = NULL, parallel.options = NULL, turbo = FALSE,
                                tol = 1e-07, LAPACK = FALSE, max.regs = NULL, print.searchinfo = TRUE,
                                plot = NULL, alarm = FALSE, ...) {
  isat.call <- sys.call()
  vcov.type <- match.arg(vcov.type)
  info.method <- match.arg(info.method)
  gof.method <- match.arg(gof.method)
  isat.args <- list(mc = mc, ar = ar, ewma = ewma, iis = iis,
                    sis = sis, tis = tis, uis = uis, uis.logical = if (is.null(uis) |
                                                                       identical(uis, FALSE)) {
                      FALSE
                    } else {
                      TRUE
                    }, blocks = blocks, ratio.threshold = ratio.threshold,
                    max.block.size = max.block.size, t.pval = t.pval, wald.pval = wald.pval,
                    vcov.type = vcov.type, do.pet = do.pet, ar.LjungB = ar.LjungB,
                    arch.LjungB = arch.LjungB, normality.JarqueB = normality.JarqueB,
                    info.method = info.method, user.diagnostics = user.diagnostics,
                    user.estimator = user.estimator, gof.function = gof.function,
                    gof.method = gof.method, include.gum = include.gum,
                    include.1cut = include.1cut, include.empty = include.empty,
                    max.paths = max.paths, parallel.options = parallel.options,
                    turbo = turbo, tol = tol, LAPACK = LAPACK, max.regs = max.regs)
  if (sis == FALSE && iis == FALSE && tis == FALSE && identical(uis,
                                                                FALSE)) {
    stop("No Indicator Selection Method was selected. Either set iis, sis or tis as TRUE or specify uis.")
  }
  if (!is.null(ar) && identical(ar, 0)) {
    ar <- NULL
  }
  if (!(is.numeric(ar) | is.null(ar))) {
    stop("The 'ar' argument must be NULL or numeric.")
  }
  y.name <- deparse(substitute(y))
  if (y.name[1] == "") {
    y.name <- "y"
  }
  if (is.null(ar)) {
    qstat.options <- c(1, 1)
  }
  else {
    qstat.options <- c(max(ar), 1)
  }
  # if (!is.null(include.gum)) {
  #   warning("The 'include.gum' argument is ignored (temporarily deprecated in isat)")
  # }
  #include.gum <- TRUE
  if (is.null(user.estimator)) {
    olsMethod <- switch(vcov.type, ordinary = 3, white = 4,
                        `newey-west` = 5)
    userEstArg <- list(name = "ols", tol = tol, LAPACK = LAPACK,
                       method = olsMethod)
    userEstArgArx <- NULL
  }
  else {
    userEstArg <- user.estimator
    userEstArgArx <- user.estimator
  }
  if (is.null(gof.function)) {
    gofFunArg <- list(name = "infocrit", method = info.method)
  }
  else {
    gofFunArg <- gof.function
  }
  if (!is.null(max.paths) && max.paths < 1) {
    stop("'max.paths' cannot be smaller than 1")
  }
  if (!is.null(parallel.options)) {
    if (is.numeric(parallel.options)) {
      clusterSpec <- parallel.options
      OScores <- detectCores()
      if (parallel.options > OScores) {
        stop("parallel.options > number of cores/threads")
      }
    }
    if (is.list(parallel.options)) {
      clusterVarlist <- parallel.options$varlist
    }
    else {
      clusterVarlist <- NULL
    }
    clusterVarlist <- c(clusterVarlist, "dropvar", "getsFun",
                        "ols", "infocrit", "diagnostics")
    if (!is.null(user.diagnostics)) {
      clusterVarlist <- c(clusterVarlist, user.diagnostics$name)
    }
    if (!is.null(user.estimator)) {
      clusterVarlist <- c(clusterVarlist, user.estimator$name)
    }
    if (!is.null(gof.function)) {
      clusterVarlist <- c(clusterVarlist, gof.function$name)
    }
  }
  mX <- gets::regressorsMean(y, mc = mc, ar = ar, ewma = ewma, mxreg = mxreg,
                             return.regressand = TRUE, return.as.zoo = TRUE, na.trim = TRUE)
  y.n <- NROW(mX)
  y.index <- zoo::index(mX)
  y.index.as.char <- as.character(y.index)
  y <- zoo::coredata(mX[, 1])
  if (NCOL(mX) == 1) {
    mX <- NULL
    mXnames <- NULL
    mXncol <- 0
    mxkeep <- NULL
  }
  else {
    mXnames <- colnames(mX)[-1]
    mX <- as.matrix(zoo::coredata(mX[, -1]))
    colnames(mX) <- mXnames
    mXncol <- NCOL(mX)
    mxkeep <- 1:mXncol
  }
  arLjungB <- NULL
  if (!is.null(ar.LjungB)) {
    arLjungB <- c(NA, ar.LjungB$pval)
    if (is.null(ar.LjungB$lag)) {
      arLjungB[1] <- qstat.options[1]
    }
    else {
      arLjungB[1] <- ar.LjungB$lag
    }
  }
  archLjungB <- NULL
  if (!is.null(arch.LjungB)) {
    archLjungB <- c(NA, arch.LjungB$pval)
    if (is.null(arch.LjungB$lag)) {
      archLjungB[1] <- qstat.options[2]
    }
    else {
      archLjungB[1] <- arch.LjungB$lag
    }
  }
  ISmatrices <- gets:::create.ISmatrices(iis = iis, sis = sis, tis = tis,
                                         uis = uis, y.n = y.n, y.index.as.char = y.index.as.char)
  if (is.list(blocks)) {
    if (length(ISmatrices) != length(blocks)) {
      stop("No. of IS matrices is unequal to length(blocks)")
    }
    blocks.is.list <- TRUE
    ISblocks <- blocks
  }
  else {
    blocks.is.list <- FALSE
    ISblocks <- list()
  }
  estimations.total <- 0
  getsFun.total <- 0
  ISfinalmodels <- list()
  for (i in 1:length(ISmatrices)) {
    result_ISMatricesLoop <- gets:::ISMatricesLoop(blocks.is.list = blocks.is.list,
                                                   ISmatrices = ISmatrices, ratio.threshold = ratio.threshold,
                                                   mXncol = mXncol, parallel.options = parallel.options,
                                                   ISblocks = ISblocks, i = i, y = y, y.n = y.n, mX = mX,
                                                   userEstArg = userEstArg, t.pval = t.pval, wald.pval = wald.pval,
                                                   do.pet = do.pet, arLjungB = arLjungB, archLjungB = archLjungB,
                                                   normality.JarqueB = normality.JarqueB, user.diagnostics = user.diagnostics,
                                                   gofFunArg = gofFunArg, gof.method = gof.method,
                                                   mxkeep = mxkeep, include.gum = include.gum, include.1cut = include.1cut,
                                                   include.empty = include.empty, max.paths = max.paths,
                                                   turbo = turbo, tol = tol, LAPACK = LAPACK, max.regs = max.regs,
                                                   print.searchinfo = print.searchinfo, clusterSpec = clusterSpec,
                                                   clusterVarlist = clusterVarlist, blocks = blocks,
                                                   max.block.size = max.block.size, mXnames = mXnames)
    ISblocks <- result_ISMatricesLoop$ISblocks
    ISfinalmodels[[i]] <- result_ISMatricesLoop$ISfinalmodel
    estimations.total <- estimations.total + result_ISMatricesLoop$estimations.counter
    getsFun.total <- getsFun.total + result_ISMatricesLoop$getsFun.counter
  }
  names(ISblocks) <- names(ISmatrices)
  if (print.searchinfo) {
    message("\n", appendLF = FALSE)
    message("GETS of union of ALL retained variables...",
            appendLF = TRUE)
  }
  if (length(ISfinalmodels) > 0) {
    mIS <- NULL
    for (i in 1:length(ISfinalmodels)) {
      isNames <- NULL
      if (!is.null(ISfinalmodels[[i]])) {
        isNames <- setdiff(ISfinalmodels[[i]], mXnames)
      }
      if (length(isNames) > 0) {
        tmp <- cbind(ISmatrices[[i]][, isNames])
        colnames(tmp) <- isNames
        mIS <- cbind(mIS, tmp)
      }
    }
    if (NCOL(cbind(mX, mIS)) >= y.n) {
      result_additional_blocksearch <- ISadditionalblocksearch(mXis = cbind(mX,
                                                                            mIS), isNames = colnames(mIS), y = y, y.n = y.n,
                                                               mX = mX, mXnames = mXnames, mxkeep = mxkeep,
                                                               ISmatrixname = "the union of indicators", indicator.set = mIS,
                                                               estimations.counter = estimations.counter, getsFun.counter = getsFun.counter,
                                                               print.searchinfo = print.searchinfo, tol = tol,
                                                               LAPACK = LAPACK, userEstArg = userEstArg, t.pval = t.pval,
                                                               gof.method = gof.method, gofFunArg = gofFunArg,
                                                               max.regs = max.regs, max.paths = max.paths,
                                                               arLjungB = arLjungB, archLjungB = archLjungB,
                                                               normality.JarqueB = normality.JarqueB, user.diagnostics = user.diagnostics,
                                                               include.gum = include.gum, include.1cut = include.1cut,
                                                               include.empty = include.empty, turbo = turbo,
                                                               do.pet = do.pet, ratio.threshold = ratio.threshold,
                                                               max.block.size = max.block.size)
      addblocksearch.names <- colnames(result_additional_blocksearch$mXis)
      mIS <- result_additional_blocksearch$mXis[, setdiff(mXnames,
                                                          addblocksearch.names)]
      estimations.counter <- result_additional_blocksearch$estimations.counter
      getsFun.counter <- result_additional_blocksearch$getsFun.counter
    }
    mXis.names <- colnames(cbind(mX, mIS))
    original.mxkeep.names <- mXis.names[mxkeep]
    mXis <- gets::dropvar(cbind(mX, mIS), tol = tol, LAPACK = LAPACK,
                          silent = !print.searchinfo)
    mXis.names.afterdropvar <- colnames(mXis)
    mxkeep.afterdropvar <- which(mXis.names.afterdropvar %in%
                                   original.mxkeep.names)
  }
  if (length(ISfinalmodels) == 0) {
    ISfinalmodels <- NULL
    if (is.null(mX)) {
      mXis <- NULL
    }
    else {
      mXis <- cbind(mX)
      colnames(mXis) <- mXnames
    }
  }
  getsis <- gets::getsFun(y, mXis, untransformed.residuals = NULL,
                          user.estimator = userEstArg, gum.result = NULL, t.pval = t.pval,
                          wald.pval = wald.pval, do.pet = do.pet, ar.LjungB = arLjungB,
                          arch.LjungB = archLjungB, normality.JarqueB = normality.JarqueB,
                          user.diagnostics = user.diagnostics, gof.function = gofFunArg,
                          gof.method = gof.method, keep = mxkeep.afterdropvar,
                          include.gum = include.gum, include.1cut = include.1cut,
                          include.empty = include.empty, max.paths = max.paths,
                          turbo = turbo, tol = tol, LAPACK = LAPACK, max.regs = max.regs,
                          print.searchinfo = print.searchinfo, alarm = FALSE)
  estimations.total <- estimations.total + getsis$no.of.estimations
  getsis$no.of.estimations <- estimations.total
  getsFun.total <- getsFun.total + 1
  getsis$no.of.getsFun.calls <- getsFun.total
  if (print.searchinfo && !is.null(getsis$messages)) {
    message(getsis$messages)
  }
  y <- zoo::zoo(y, order.by = y.index)
  if (is.null(getsis$specific.spec)) {
    mXisNames <- NULL
    mXis <- NULL
  }
  else {
    mXisNames <- colnames(mXis)[getsis$specific.spec]
    mXis <- cbind(mXis[, getsis$specific.spec])
    colnames(mXis) <- mXisNames
    mXis <- zoo::zoo(mXis, order.by = y.index)
  }
  if (is.null(normality.JarqueB)) {
    normalityArg <- FALSE
  }
  else {
    normalityArg <- as.numeric(normality.JarqueB)
  }
  tmpmc <- getOption("mc.warning")
  options(mc.warning = FALSE)
  mod <- gets::arx(y, mc = FALSE, mxreg = mXis, vcov.type = vcov.type,
                   qstat.options = qstat.options, normality.JarqueB = normalityArg,
                   user.estimator = userEstArgArx, user.diagnostics = user.diagnostics,
                   tol = tol, LAPACK = LAPACK, plot = FALSE)
  mod$call <- NULL
  options(mc.warning = tmpmc)
  ISnames <- setdiff(mXisNames, mXnames)
  if (length(ISnames) == 0) {
    ISnames <- NULL
  }
  colnames(mod$aux$mX) <- mod$aux$mXnames
  getsis$gets.type <- "isat"
  getsis$call <- isat.call
  getsis <- c(list(ISfinalmodels = ISfinalmodels, ISnames = ISnames),
              getsis, mod)
  getsis$aux$t.pval <- t.pval
  class(getsis) <- "isat"
  if (alarm) {
    alarm()
  }
  if (is.null(plot)) {
    plot <- getOption("plot")
    if (is.null(plot)) {
      plot <- FALSE
    }
  }
  if (plot) {
    plot.isat(getsis, coef.path = TRUE)
  }
  getsis$aux$args <- isat.args
  if (isat.args$iis && identical(userEstArg$name, "ols") &&
      !any(isat.args$sis, isat.args$tis, isat.args$uis.logical)) {
    getsis$outlier.proportion.test <- gets::outliertest(getsis)
    getsis$outlier.distortion.test <- gets::distorttest(getsis)
  }
  return(getsis)
}
