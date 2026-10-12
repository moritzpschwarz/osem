#' Convert an estimated OSEM/GETS model to a LaTeX equation
#'
#' @param model An estimated model object. Can be an `arx`/`isat`-style object,
#'   or an `osem` object if `lhs_name` identifies the dependent variable.
#' @param digits Integer. Number of decimal places for coefficients.
#' @param lhs_name Optional character scalar. If `model` is an `osem` object,
#'   this should be the dependent variable whose module equation should be shown.
#' @param show_indicators Logical. Print IIS/SIS/TIS indicators individually?
#' @param show_seasonals Logical. Print seasonal dummies individually?
#' @param show_stars Logical. Add significance stars based on model p-values?
#' @param width Optional integer. Approximate character width for line breaks.
#'
#' @return Invisibly returns the LaTeX equation as a character scalar and prints it.
#'
#' @export
#' @examples
#'
#' spec <- dplyr::tibble(
#' type = c(
#'  "d",
#'  "d",
#'  "n"
#'),
#'dependent = c(
#'  "StatDiscrep",
#'  "TOTS",
#'  "Import"
#'),
#'independent = c(
#'  "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
#'  "GValueAdd + Import",
#'  "FinConsExpHH + GCapitalForm"
#')
#')
#'
#' model <- run_model(specification = spec,
#'          primary_source = "local",
#'          input = sample_input)
#'
#'
#' equation_latex(model)
#'
#'
equation_latex <- function(model,
                           digits = 2,
                           lhs_name = NULL,
                           show_indicators = TRUE,
                           show_seasonals = TRUE,
                           show_stars = FALSE,
                           width = 70,
                           print = TRUE) {

  # ---------------------------------------------------------------------------
  # 1. If a full OSEM object is supplied, extract the requested module model
  # ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------
  # 1. If a full OSEM object is supplied
  # ---------------------------------------------------------------------------

  if (inherits(model, "osem")) {

    if (!"module_collection" %in% names(model)) {
      stop("The supplied `osem` object has no `module_collection` element.")
    }

    if (!all(c("dependent", "type", "model") %in% names(model$module_collection))) {
      stop("`model$module_collection` must contain `dependent`, `type`, and `model` columns.")
    }

    # If lhs_name is NULL, print all estimated equations.
    if (is.null(lhs_name)) {

      estimated_modules <- model$module_collection %>%
        dplyr::filter(.data$type == "n")

      if (nrow(estimated_modules) == 0) {
        stop("No estimated equations found. Identity equations cannot currently be printed by `equation_latex()`.")
      }

      equations <- purrr::map2_chr(
        estimated_modules$model,
        estimated_modules$dependent,
        ~ equation_latex(
          model = .x,
          digits = digits,
          lhs_name = .y,
          show_indicators = show_indicators,
          show_seasonals = show_seasonals,
          show_stars = show_stars,
          width = width,
          print = FALSE
        )
      )

      out <- paste(equations, collapse = "\n\n")

      if (print) {
        cat(out)
      }

      return(invisible(out))
    }

    # If lhs_name is supplied, check whether it exists.
    selected_row <- model$module_collection %>%
      dplyr::filter(.data$dependent == lhs_name)

    if (nrow(selected_row) == 0) {
      stop("No module found for lhs_name = ", lhs_name, ".")
    }

    # Fail cleanly if the selected equation is an identity.
    if (!identical(selected_row$type[[1]], "n")) {
      stop(
        "`", lhs_name, "` is an identity equation, not an estimated equation. ",
        "`equation_latex()` currently only supports estimated modules."
      )
    }

    model <- selected_row$model[[1]]
  }
  original_model <- model

  # ---------------------------------------------------------------------------
  # 2. Extract coefficients and p-values robustly
  # ---------------------------------------------------------------------------

  coefs <- stats::coef(model)
  clean_coef_names <- function(x) {
    x <- gsub("`", "", x, fixed = TRUE)
    x
  }

  cn <- clean_coef_names(names(coefs))
  names(coefs) <- cn

  if (is.null(coefs) || length(coefs) == 0) {
    stop("No coefficients found. `stats::coef(model)` returned empty output.")
  }

  if (inherits(model, "lm")) {
    pvals <- summary(model)$coefficients[, 4]
  } else {
    pvals <- model$mean.results$`p-value`
  }

  if (is.null(pvals)) {
    pvals <- rep(NA_real_, length(coefs))
  }

  names(pvals) <- cn


  if (is.null(pvals)) {
    pvals <- rep(NA_real_, length(coefs))
  }

  if (is.null(names(pvals))) {
    names(pvals) <- names(coefs)
  }

  fmt <- function(x) {
    sprintf(paste0("%.", digits, "f"), x)
  }

  # ---------------------------------------------------------------------------
  # 3. Significance stars
  # ---------------------------------------------------------------------------

  if (show_stars) {

    stars_raw <- as.character(
      stats::symnum(
        pvals,
        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
        symbols = c("***", "**", "*", "")
      )
    )

    stars <- ifelse(
      stars_raw == "" | is.na(stars_raw),
      "",
      paste0("^{", stars_raw, "}")
    )

    names(stars) <- names(pvals)

  } else {
    stars <- NULL
  }

  get_stars <- function(term_names) {
    if (is.null(stars)) {
      return(rep("", length(term_names)))
    }

    out <- stars[term_names]
    out[is.na(out)] <- ""
    unname(out)
  }

  # ---------------------------------------------------------------------------
  # 4. Formatting helpers
  # ---------------------------------------------------------------------------

  escape_latex_text <- function(x) {
    gsub("_", "\\_", x, fixed = TRUE)
  }

  clean_var <- function(x, force_t = TRUE) {

    vapply(
      x,
      FUN.VALUE = character(1),
      function(v) {

        lagged <- grepl("^L1\\.", v)
        diffed <- grepl("^D\\.", v)

        v <- sub("^L1\\.", "", v)
        v <- sub("^D\\.", "", v)

        if (startsWith(v, "ln.")) {
          v <- sub("^ln\\.", "", v)
          v <- escape_latex_text(v)
          out <- paste0("\\ln(\\text{", v, "})")
        } else {
          v <- escape_latex_text(v)
          out <- paste0("\\text{", v, "}")
        }

        if (force_t) {
          if (lagged) {
            out <- paste0(out, "_{t-1}")
          } else {
            out <- paste0(out, "_t")
          }
        }

        if (diffed) {
          out <- paste0("\\Delta ", out)
        }

        out
      }
    )
  }

  get_lhs_name <- function(original_model, model, lhs_name = NULL) {

    if (!is.null(lhs_name)) {
      return(lhs_name)
    }

    if (!is.null(original_model$aux$y.name)) {
      return(original_model$aux$y.name)
    }

    if (!is.null(original_model$aux$y_name)) {
      return(original_model$aux$y_name)
    }

    f <- tryCatch(stats::formula(model), error = function(e) NULL)

    if (!is.null(f)) {
      return(as.character(f)[2])
    }

    "y"
  }

  format_indicator <- function(x) {

    vapply(
      x,
      FUN.VALUE = character(1),
      function(z) {

        type <- toupper(substr(z, 1, 3))
        date_part <- sub("^(iis|sis|tis)", "", z)
        date_part <- gsub("[^0-9]", "-", date_part)
        date_part <- gsub("-+", "-", date_part)
        date_part <- sub("-$", "", date_part)

        d <- suppressWarnings(as.Date(date_part))

        if (is.na(d)) {
          z <- escape_latex_text(z)
          return(paste0("\\text{", z, "}_t"))
        }

        year <- format(d, "%Y")
        month <- as.integer(format(d, "%m"))
        q <- ceiling(month / 3)

        paste0(type, "_{", year, "Q", q, "}")
      }
    )
  }

  signed_terms <- function(beta,
                           tex_names,
                           stars = NULL) {

    if (length(beta) == 0) {
      return(character(0))
    }

    if (is.null(stars) || length(stars) == 0) {
      stars <- rep("", length(beta))
    }

    paste0(
      ifelse(beta >= 0, "+", "-"),
      fmt(abs(beta)),
      stars,
      "\\,",
      tex_names
    )
  }

  # ---------------------------------------------------------------------------
  # 5. Classify coefficient types
  # ---------------------------------------------------------------------------

  is_intercept <- cn == "(Intercept)" | cn == "mconst"
  is_indicator <- grepl("^(iis|sis|tis)\\d{4}", cn)
  is_seasonal <- grepl("^q_[0-9]+$", cn)
  is_diff <- grepl("^D\\.", cn)
  is_lag_level <- grepl("^L1\\.", cn)

  pos <- seq_along(coefs)
  names(pos) <- cn

  lhs_name <- get_lhs_name(original_model, model, lhs_name)
  lhs_tex <- clean_var(lhs_name)

  is_ecm <- any(is_lag_level)

  rhs_extra <- character(0)

  # ---------------------------------------------------------------------------
  # 6. Seasonals, with stars
  # ---------------------------------------------------------------------------

  if (any(is_seasonal)) {

    seas <- coefs[is_seasonal]
    seas_pos <- which(is_seasonal)

    if (show_seasonals) {
      rhs_extra <- c(
        rhs_extra,
        paste0(
          ifelse(seas >= 0, "+", "-"),
          fmt(abs(seas)),
          get_stars(names(seas)),
          "\\,Q_{",
          sub("^q_", "", names(seas)),
          ",t}"
        )
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Indicators, with stars
  # ---------------------------------------------------------------------------

  if (any(is_indicator)) {

    inds <- coefs[is_indicator]
    inds_pos <- which(is_indicator)

    if (show_indicators) {
      rhs_extra <- c(
        rhs_extra,
        paste0(
          ifelse(inds >= 0, "+", "-"),
          fmt(abs(inds)),
          get_stars(names(inds)),
          "\\,",
          format_indicator(names(inds))
        )
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 8. Intercept, with stars
  # ---------------------------------------------------------------------------

  intercept_term <- character(0)

  if (any(is_intercept)) {

    intercept <- coefs[is_intercept]

    intercept_term <- paste0(
      ifelse(intercept >= 0, "+", "-"),
      fmt(abs(intercept)),
      get_stars(names(intercept))
    )
  }

  # ---------------------------------------------------------------------------
  # 9. ECM or non-ECM representation
  # ---------------------------------------------------------------------------

  if (is_ecm) {

    lag_coefs <- coefs[is_lag_level]
    lag_pos <- which(is_lag_level)

    # Assumption: first lagged level term is the dependent variable level.
    alpha <- lag_coefs[1]
    alpha_pos <- lag_pos[1]
    level_x <- lag_coefs[-1]
    level_x_pos <- lag_pos[-1]

    alpha_star <- get_stars(names(alpha))

    ecm_block <- paste0(
      fmt(alpha),
      alpha_star,
      "\\left(",
      clean_var(names(alpha))
    )

    if (length(level_x) > 0) {

      beta <- -level_x / alpha

      for (i in seq_along(beta)) {

        sign <- ifelse(beta[i] >= 0, "-", "+")

        # Deliberately no stars on transformed long-run coefficients.
        ecm_block <- paste0(
          ecm_block,
          sign,
          fmt(abs(beta[i])),
          "\\,",
          clean_var(names(beta)[i])
        )
      }
    }

    ecm_block <- paste0(ecm_block, "\\right)")

    short_run <- coefs[
      is_diff &
        !is_indicator &
        !is_seasonal &
        !is_intercept
    ]

    short_pos <- which(
      is_diff &
        !is_indicator &
        !is_seasonal &
        !is_intercept
    )

    short_terms <- signed_terms(
      short_run,
      clean_var(names(short_run)),
      stars = get_stars(names(short_run))
    )

    rhs <- c(
      ecm_block,
      short_terms,
      intercept_term,
      rhs_extra,
      "+u_t"
    )

  } else {

    keep <- !is_indicator &
      !is_seasonal &
      !is_intercept

    main <- coefs[keep]
    main_pos <- which(keep)

    main_terms <- signed_terms(
      main,
      clean_var(names(main)),
      stars = get_stars(names(main))
    )

    rhs <- c(
      intercept_term,
      main_terms,
      rhs_extra,
      "+u_t"
    )

    rhs[1] <- sub("^\\+", "", rhs[1])
  }

  # ---------------------------------------------------------------------------
  # 10. Optional line breaking
  # ---------------------------------------------------------------------------

  if (!is.null(width)) {

    lines <- list()
    current <- character()

    for (term in rhs) {

      candidate <- paste(c(current, term), collapse = " ")

      if (nchar(candidate) > width && length(current) > 0) {
        lines[[length(lines) + 1]] <- current
        current <- term
      } else {
        current <- c(current, term)
      }
    }

    lines[[length(lines) + 1]] <- current

    line_text <- vapply(
      lines,
      paste,
      collapse = " ",
      FUN.VALUE = character(1)
    )

    rhs_text <- paste(
      line_text,
      collapse = " \\\\\n&\\quad "
    )

    eq <- paste0(
      "$$\n\\begin{aligned}\n",
      lhs_tex,
      " &= ",
      rhs_text,
      "\n\\end{aligned}\n$$"
    )

  } else {

    eq <- paste0(
      "$$\n",
      lhs_tex,
      " = ",
      paste(rhs, collapse = " "),
      "\n$$"
    )
  }

  if (print) {
    cat(eq)
  }

  invisible(eq)
}
