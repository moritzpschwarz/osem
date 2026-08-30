#' Compress retained IIS/SIS indicator patterns in an estimated gets/isat model
#'
#' This function post-processes an already selected `gets::isat` model. It looks
#' for two common indicator patterns:
#'
#' 1. consecutive IIS indicators with the same sign, which may be more
#'    parsimoniously represented as a single SIS indicator;
#' 2. adjacent SIS indicators with opposite signs and similar magnitudes, which
#'    may be more parsimoniously represented as a single IIS indicator.
#'
#' Candidate replacements are accepted only if they improve the selected
#' information criterion, by default BIC.
#'
#' @param final_model An estimated `gets::isat` model object.
#' @param min_iis_run_length Integer. Minimum number of consecutive same-sign IIS
#'   indicators required before proposing an IIS-to-SIS compression. Default is 3.
#' @param same_sign_ratio Numeric. Maximum allowed ratio between the largest and
#'   smallest absolute IIS coefficient within a same-sign run. Default is 3.
#' @param sis_reversal_tolerance Numeric. Tolerance for treating adjacent
#'   opposite-sign SIS indicators as approximately cancelling. The pair is
#'   considered a reversal if `abs(beta1 + beta2) <= tolerance * max(abs(beta1), abs(beta2))`.
#'   Default is 0.25.
#' @param criterion Character. Either `"BIC"` or `"AIC"`. Default is `"BIC"`.
#' @param iterate Logical. If TRUE, repeatedly searches for further improvements
#'   after each accepted compression. Default is TRUE.
#' @param max_iter Integer. Maximum number of compression iterations. Default is 10.
#' @param require_diagnostics_not_worse Logical. If TRUE, do not accept a
#'   compression if available AR/ARCH diagnostic p-values get worse than
#'   `diagnostic_pval_floor`. Default is FALSE.
#' @param diagnostic_pval_floor Numeric. Minimum acceptable diagnostic p-value
#'   when `require_diagnostics_not_worse = TRUE`. Default is 0.01.
#' @param verbose Logical. If TRUE, print accepted modifications. Default is FALSE.
#'
#' @return A list with the original model, compressed model, accepted
#'   modifications, all attempted candidates, and criterion values.
#'
#' @keywords internal
compress_indicators <- function(final_model,
                                min_iis_run_length = 3,
                                same_sign_ratio = 3,
                                sis_reversal_tolerance = 0.25,
                                criterion = c("BIC", "AIC"),
                                iterate = TRUE,
                                max_iter = 10,
                                require_diagnostics_not_worse = FALSE,
                                diagnostic_pval_floor = 0.01,
                                verbose = FALSE) {
  criterion <- match.arg(criterion)

  if (is.null(final_model$aux$y) ||
      is.null(final_model$aux$mX) ||
      is.null(final_model$aux$mXnames) ||
      is.null(final_model$aux$y.index)) {
    stop(
      "final_model must contain aux$y, aux$mX, aux$mXnames, and aux$y.index.",
      call. = FALSE
    )
  }

  y <- as.numeric(final_model$aux$y)
  y_index <- final_model$aux$y.index

  current_model <- final_model
  accepted_modifications <- dplyr::tibble()
  all_candidates <- dplyr::tibble()

  old_criterion <- if(criterion == "BIC") {
    stats::BIC(current_model)
  } else if (criterion == "AIC"){
    stats::AIC(current_model)
  } else {
    stop("Criterion not yet implemented. Choose either AIC or BIC.")
  }

  iter <- 1
  improvement_found <- TRUE

  while (isTRUE(improvement_found) && iter <= max_iter) {
    improvement_found <- FALSE

    indicator_table <- extract_indicator_table(current_model)

    if (NROW(indicator_table) == 0) {
      break
    }

    candidates <- dplyr::bind_rows(
      create_iis_to_sis_candidates(
        indicator_table = indicator_table,
        y_index = y_index,
        min_iis_run_length = min_iis_run_length,
        same_sign_ratio = same_sign_ratio
      ),
      create_sis_to_iis_candidates(
        indicator_table = indicator_table,
        y_index = y_index,
        tolerance = sis_reversal_tolerance
      )
    )

    if (NROW(candidates) == 0) {
      break
    }

    evaluated <- evaluate_indicator_candidates(
      model_object = current_model,
      candidates = candidates,
      criterion = criterion,
      require_diagnostics_not_worse = require_diagnostics_not_worse,
      diagnostic_pval_floor = diagnostic_pval_floor
    )

    evaluated <- evaluated %>%
      dplyr::mutate(iteration = iter)

    all_candidates <- dplyr::bind_rows(all_candidates, evaluated)

    admissible <- evaluated %>%
      dplyr::filter(.data$admissible) %>%
      dplyr::arrange(.data$criterion_new)

    if (NROW(admissible) == 0) {
      break
    }

    best_candidate <- admissible[1, ]

    if (isTRUE(best_candidate$criterion_new < old_criterion)) {
      candidate_model <- attr(best_candidate, "candidate_models")[[best_candidate$candidate_id]]

      current_model <- candidate_model
      improvement_found <- TRUE

      accepted_row <- best_candidate %>%
        dplyr::select(
          "iteration",
          "candidate_id",
          "type",
          "remove_terms",
          "add_terms",
          "criterion",
          "criterion_old",
          "criterion_new",
          "criterion_delta",
          "rss_old",
          "rss_new",
          "k_old",
          "k_new",
          "n"
        )

      accepted_modifications <- dplyr::bind_rows(
        accepted_modifications,
        accepted_row
      )

      old_criterion <- best_candidate$criterion_new

      if (isTRUE(verbose)) {
        message(
          "Accepted indicator compression: ",
          best_candidate$type,
          " | remove: ",
          paste(unlist(best_candidate$remove_terms), collapse = ", "),
          " | add: ",
          paste(unlist(best_candidate$add_terms), collapse = ", "),
          " | ",
          criterion,
          ": ",
          round(best_candidate$criterion_old, 4),
          " -> ",
          round(best_candidate$criterion_new, 4)
        )
      }
    }

    if (!iterate) {
      break
    }

    iter <- iter + 1
  }

  new_criterion <- if(criterion == "BIC") {
    stats::BIC(current_model)
  } else if (criterion == "AIC"){
    stats::AIC(current_model)
  } else {
    stop("Criterion not yet implemented. Choose either AIC or BIC.")
  }

  out <- list(
    original_model = final_model,
    compressed_model = current_model,
    accepted = NROW(accepted_modifications) > 0,
    modifications = accepted_modifications,
    candidates = all_candidates,
    criterion = criterion,
    criterion_original = old_criterion,
    criterion_final = new_criterion
  )

  return(out)
}


#' Extract retained IIS/SIS indicators from a gets/isat object
#'
#' @keywords internal
extract_indicator_table <- function(model_object) {
  coefs <- coef(model_object)

  if (is.null(coefs) || length(coefs) == 0) {
    return(dplyr::tibble())
  }

  out <- tibble::tibble(
    term = names(coefs),
    coefficient = as.numeric(coefs)
  ) %>%
    dplyr::filter(grepl("^(iis|sis)", .data$term)) %>%
    dplyr::mutate(
      type = toupper(substr(.data$term, 1, 3)),
      date_chr = sub("^(iis|sis)", "", .data$term),
      date = as.Date(.data$date_chr),
      sign = sign(.data$coefficient),
      abs_coefficient = abs(.data$coefficient)
    ) %>%
    dplyr::filter(!is.na(.data$date)) %>%
    dplyr::arrange(.data$date, .data$type)

  out
}


#' Create IIS-to-SIS compression candidates
#'
#' @keywords internal
create_iis_to_sis_candidates <- function(indicator_table,
                                         y_index,
                                         min_iis_run_length = 3,
                                         same_sign_ratio = 3) {
  iis <- indicator_table %>%
    dplyr::filter(.data$type == "IIS") %>%
    dplyr::arrange(.data$date)

  if (NROW(iis) < min_iis_run_length) {
    return(empty_candidate_tbl())
  }

  date_pos <- tibble::tibble(
    date = as.Date(y_index),
    position = seq_along(y_index)
  )

  iis <- iis %>%
    dplyr::left_join(date_pos, by = "date") %>%
    dplyr::filter(!is.na(.data$position)) %>%
    dplyr::arrange(.data$position)

  if (NROW(iis) < min_iis_run_length) {
    return(empty_candidate_tbl())
  }

  iis <- iis %>%
    dplyr::mutate(
      new_run = dplyr::case_when(
        dplyr::row_number() == 1 ~ TRUE,
        .data$sign != dplyr::lag(.data$sign) ~ TRUE,
        .data$position != dplyr::lag(.data$position) + 1 ~ TRUE,
        TRUE ~ FALSE
      ),
      run_id = cumsum(.data$new_run)
    )

  runs <- iis %>%
    dplyr::group_by(.data$run_id) %>%
    dplyr::summarise(
      type = "IIS_to_SIS",
      start_date = min(.data$date),
      end_date = max(.data$date),
      start_position = min(.data$position),
      end_position = max(.data$position),
      run_length = dplyr::n(),
      run_sign = dplyr::first(.data$sign),
      max_abs_coef = max(.data$abs_coefficient, na.rm = TRUE),
      min_abs_coef = min(.data$abs_coefficient, na.rm = TRUE),
      remove_terms = list(.data$term),
      add_terms = list(paste0("sis", min(.data$date))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      abs_coef_ratio = dplyr::if_else(
        .data$min_abs_coef > 0,
        .data$max_abs_coef / .data$min_abs_coef,
        Inf
      )
    ) %>%
    dplyr::filter(
      .data$run_length >= min_iis_run_length,
      .data$abs_coef_ratio <= same_sign_ratio
    ) %>%
    dplyr::mutate(
      candidate_id = paste0("IIS_to_SIS_", dplyr::row_number())
    ) %>%
    dplyr::select(
      "candidate_id",
      "type",
      "start_date",
      "end_date",
      "start_position",
      "end_position",
      "run_length",
      "remove_terms",
      "add_terms",
      "abs_coef_ratio"
    )

  if (NROW(runs) == 0) {
    return(empty_candidate_tbl())
  }

  runs
}


#' Create SIS-to-IIS compression candidates
#'
#' @keywords internal
create_sis_to_iis_candidates <- function(indicator_table,
                                         y_index,
                                         tolerance = 0.25) {
  sis <- indicator_table %>%
    dplyr::filter(.data$type == "SIS") %>%
    dplyr::arrange(.data$date)

  if (NROW(sis) < 2) {
    return(empty_candidate_tbl())
  }

  date_pos <- tibble::tibble(
    date = as.Date(y_index),
    position = seq_along(y_index)
  )

  sis <- sis %>%
    dplyr::left_join(date_pos, by = "date") %>%
    dplyr::filter(!is.na(.data$position)) %>%
    dplyr::arrange(.data$position)

  if (NROW(sis) < 2) {
    return(empty_candidate_tbl())
  }

  out <- vector("list", length = NROW(sis) - 1)

  for (i in seq_len(NROW(sis) - 1)) {
    beta_1 <- sis$coefficient[i]
    beta_2 <- sis$coefficient[i + 1]

    adjacent <- sis$position[i + 1] == sis$position[i] + 1
    opposite_sign <- sign(beta_1) == -sign(beta_2)
    similar_magnitude <- abs(beta_1 + beta_2) <=
      tolerance * max(abs(beta_1), abs(beta_2))

    if (isTRUE(adjacent) &&
        isTRUE(opposite_sign) &&
        isTRUE(similar_magnitude)) {
      out[[i]] <- tibble::tibble(
        candidate_id = paste0("SIS_to_IIS_", i),
        type = "SIS_to_IIS",
        start_date = sis$date[i],
        end_date = sis$date[i + 1],
        start_position = sis$position[i],
        end_position = sis$position[i + 1],
        run_length = 2L,
        remove_terms = list(c(sis$term[i], sis$term[i + 1])),
        add_terms = list(paste0("iis", sis$date[i])),
        abs_coef_ratio = max(abs(beta_1), abs(beta_2)) /
          min(abs(beta_1), abs(beta_2))
      )
    }
  }

  out <- dplyr::bind_rows(out)

  if (NROW(out) == 0) {
    return(empty_candidate_tbl())
  }

  out
}


#' Evaluate indicator compression candidates
#'
#' @keywords internal
evaluate_indicator_candidates <- function(model_object,
                                          candidates,
                                          criterion = c("BIC", "AIC"),
                                          require_diagnostics_not_worse = FALSE,
                                          diagnostic_pval_floor = 0.01) {
  criterion <- match.arg(criterion)

  if (NROW(candidates) == 0) {
    return(dplyr::tibble())
  }

  y <- as.numeric(model_object$aux$y)

  old_stats <- estimate_candidate_as_isat(
    model_object = model_object,
    X = as.matrix(model_object$aux$mX),
    Xnames = model_object$aux$mXnames,
    criterion = criterion
  )

  candidate_models <- vector("list", NROW(candidates))
  evaluated_rows <- vector("list", NROW(candidates))

  for (i in seq_len(NROW(candidates))) {
    remove_terms <- unlist(candidates$remove_terms[[i]])
    add_terms <- unlist(candidates$add_terms[[i]])

    design <- build_candidate_design(
      model_object = model_object,
      remove_terms = remove_terms,
      add_terms = add_terms
    )

    if (is.null(design) || NCOL(design$X) == 0) {
      evaluated_rows[[i]] <- candidates[i, ] %>%
        dplyr::mutate(
          criterion = criterion,
          criterion_old = old_stats[[tolower(criterion)]],
          criterion_new = NA_real_,
          criterion_delta = NA_real_,
          rss_old = old_stats$rss,
          rss_new = NA_real_,
          k_old = old_stats$k,
          k_new = NA_integer_,
          n = old_stats$n,
          admissible = FALSE,
          rejection_reason = "Candidate design could not be constructed."
        )
      next
    }

    new_stats <- estimate_candidate_as_isat(
      model_object = model_object,
      X = design$X,
      Xnames = design$Xnames,
      criterion = criterion
    )

    candidate_model <- new_stats$model

    diagnostics_ok <- TRUE
    rejection_reason <- NA_character_

    if (isTRUE(require_diagnostics_not_worse)) {
      diagnostics_ok <- candidate_diagnostics_ok(
        candidate_model = candidate_model,
        diagnostic_pval_floor = diagnostic_pval_floor
      )

      if (!diagnostics_ok) {
        rejection_reason <- paste0(
          "Candidate rejected because at least one diagnostic p-value is below ",
          diagnostic_pval_floor,
          "."
        )
      }
    }

    admissible <- isTRUE(new_stats[[tolower(criterion)]] < old_stats[[tolower(criterion)]]) &&
      isTRUE(diagnostics_ok)

    if (!admissible && is.na(rejection_reason)) {
      rejection_reason <- paste0(criterion, " did not improve.")
    }

    candidate_models[[candidates$candidate_id[i]]] <- candidate_model

    evaluated_rows[[i]] <- candidates[i, ] %>%
      dplyr::mutate(
        criterion = criterion,
        criterion_old = old_stats[[tolower(criterion)]],
        criterion_new = new_stats[[tolower(criterion)]],
        criterion_delta = new_stats[[tolower(criterion)]] -
          old_stats[[tolower(criterion)]],
        rss_old = old_stats$rss,
        rss_new = new_stats$rss,
        k_old = old_stats$k,
        k_new = new_stats$k,
        n = old_stats$n,
        admissible = admissible,
        rejection_reason = rejection_reason
      )
  }

  out <- dplyr::bind_rows(evaluated_rows)
  attr(out, "candidate_models") <- candidate_models

  out
}


#' Build a candidate design matrix by removing and adding indicator terms
#'
#' @keywords internal
build_candidate_design <- function(model_object,
                                   remove_terms,
                                   add_terms) {
  X <- as.matrix(model_object$aux$mX)
  Xnames <- model_object$aux$mXnames
  y_index <- as.Date(model_object$aux$y.index)

  colnames(X) <- Xnames

  keep_cols <- setdiff(Xnames, remove_terms)

  X_new <- X[, keep_cols, drop = FALSE]
  Xnames_new <- keep_cols

  for (term in add_terms) {
    if (term %in% Xnames_new) {
      next
    }

    new_col <- make_indicator_column(
      term = term,
      y_index = y_index
    )

    if (is.null(new_col)) {
      next
    }

    X_new <- cbind(X_new, new_col)
    Xnames_new <- c(Xnames_new, term)
    colnames(X_new) <- Xnames_new
  }

  qr_rank <- qr(X_new)$rank

  if (qr_rank < NCOL(X_new)) {
    return(NULL)
  }

  list(
    X = X_new,
    Xnames = Xnames_new
  )
}


#' Construct IIS/SIS indicator column from term name and model index
#'
#' @keywords internal
make_indicator_column <- function(term, y_index) {
  if (!grepl("^(iis|sis)", term)) {
    return(NULL)
  }

  type <- substr(term, 1, 3)
  date <- as.Date(sub("^(iis|sis)", "", term))

  if (is.na(date)) {
    return(NULL)
  }

  if (identical(type, "iis")) {
    return(as.numeric(y_index == date))
  }

  if (identical(type, "sis")) {
    return(as.numeric(y_index >= date))
  }

  NULL
}



estimate_candidate_as_isat <- function(model_object,
                                       X,
                                       Xnames,
                                       criterion = c("BIC", "AIC"),
                                       vcov.type = NULL,
                                       normality.JarqueB = NULL,
                                       user.estimator = NULL,
                                       user.diagnostics = NULL,
                                       tol = NULL,
                                       LAPACK = NULL) {
  criterion <- match.arg(criterion)

  y <- model_object$aux$y
  y_index <- model_object$aux$y.index
  y_name <- model_object$aux$y.name

  X <- as.matrix(X)
  colnames(X) <- Xnames

  if (is.null(vcov.type)) {vcov.type <- model_object$aux$vcov.type}
  if (is.null(tol)) {tol <- model_object$aux$tol}
  if (is.null(LAPACK)) {LAPACK <- model_object$aux$LAPACK}
  if (is.null(user.estimator)) {user.estimator <- model_object$aux$user.estimator}
  if (is.null(user.diagnostics)) {user.diagnostics <- model_object$aux$user.diagnostics}
  if (is.null(vcov.type)) {vcov.type <- "ordinary"}
  if (is.null(tol)) {tol <- 1e-07}
  if (is.null(LAPACK)) {LAPACK <- FALSE}

  y_zoo <- zoo::zoo(as.numeric(y), order.by = y_index)
  X_zoo <- zoo::zoo(X, order.by = y_index)

  dummy <- matrix(0,nrow = NROW(X_zoo),ncol = 1)

  candidate_model <- gets::isat(
    y = y_zoo,
    mxreg = X_zoo,
    mc = FALSE,
    iis = FALSE,
    sis = FALSE,
    tis = FALSE,
    uis = list(dummy),
    print.searchinfo = FALSE,
    additional.block.search = TRUE
  )

  candidate_model$ISnames <- grep("^(iis|tis|sis)",Xnames, value = TRUE)
  candidate_model$aux$y.name <- model_object$aux$y.name
  candidate_model$call$uis <- NULL

  bic <- stats::BIC(candidate_model)
  aic <- stats::AIC(candidate_model)

  list(
    model = candidate_model,
    bic = bic,
    aic = aic,
    criterion = if (identical(criterion, "BIC")) bic else aic,
    rss = candidate_model$rss,
    logl = candidate_model$logl,
    n = candidate_model$n,
    k = candidate_model$k
  )
}

#' Basic diagnostic guard for compressed candidates
#'
#' This intentionally only checks diagnostics if they are already available in
#' the object. It is conservative but does not rerun gets diagnostics.
#'
#' @keywords internal
candidate_diagnostics_ok <- function(candidate_model,
                                     diagnostic_pval_floor = 0.01) {
  diagnostics <- candidate_model$specific.diagnostics

  if (is.null(diagnostics)) {
    return(TRUE)
  }

  if (!"p-value" %in% colnames(diagnostics)) {
    return(TRUE)
  }

  pvals <- diagnostics[, "p-value"]
  all(is.na(pvals) | pvals >= diagnostic_pval_floor)
}


#' Empty candidate tibble with stable columns
#'
#' @keywords internal
empty_candidate_tbl <- function() {
  tibble::tibble(
    candidate_id = character(),
    type = character(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    start_position = integer(),
    end_position = integer(),
    run_length = integer(),
    remove_terms = list(),
    add_terms = list(),
    abs_coef_ratio = numeric()
  )
}
