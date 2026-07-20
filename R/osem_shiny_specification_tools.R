# OSEM Shiny specification-builder helpers ---------------------------------

osem_shiny_spec_scalar <- function(value, default = "") {
  value <- as.character(value %||% character())
  if (length(value) == 0L || is.na(value[[1L]])) {
    return(as.character(default)[[1L]])
  }
  trimws(value[[1L]])
}

osem_shiny_spec_split_plus <- function(value) {
  value <- as.character(value %||% "")
  value[is.na(value)] <- ""
  if (!nzchar(trimws(value[[1L]]))) return(character())
  out <- unlist(strsplit(value[[1L]], "\\+", perl = TRUE), use.names = FALSE)
  out <- trimws(out)
  out <- out[nzchar(out)]
  unique(out)
}

osem_shiny_spec_split_lag <- function(value) {
  value <- as.character(value %||% "")
  value[is.na(value)] <- ""
  if (!nzchar(trimws(value[[1L]]))) return(character())
  out <- unlist(strsplit(value[[1L]], ",", fixed = TRUE), use.names = FALSE)
  out <- trimws(out)
  out <- out[nzchar(out)]
  unique(out)
}

osem_shiny_spec_join_plus <- function(values) {
  values <- trimws(as.character(values %||% character()))
  values <- values[!is.na(values) & nzchar(values)]
  paste(unique(values), collapse = " + ")
}

osem_shiny_spec_join_lag <- function(values) {
  values <- trimws(as.character(values %||% character()))
  values <- values[!is.na(values) & nzchar(values)]
  paste(unique(values), collapse = ", ")
}

osem_shiny_spec_formula_tokens <- function(value) {
  value <- trimws(as.character(value %||% ""))
  value[is.na(value)] <- ""
  if (!nzchar(value[[1L]])) return(character())

  cleaned <- gsub("[()]", " ", value[[1L]])
  pieces <- unlist(
    strsplit(cleaned, "\\s*(?:\\+|-|\\*|/|\\^)\\s*", perl = TRUE),
    use.names = FALSE
  )
  pieces <- trimws(pieces)
  pieces <- pieces[nzchar(pieces)]
  pieces <- pieces[grepl("^[A-Za-z][A-Za-z0-9_.]*$", pieces)]
  pieces <- setdiff(pieces, c("NA", "NaN", "Inf", "TRUE", "FALSE"))
  unique(pieces)
}

osem_shiny_spec_variable_choices <- function(dictionary,
                                             specification = NULL,
                                             include = character()) {
  dictionary_variables <- if (
    is.data.frame(dictionary) && "model_varname" %in% names(dictionary)
  ) {
    as.character(dictionary$model_varname)
  } else {
    character()
  }
  specification_variables <- if (
    is.data.frame(specification) && "dependent" %in% names(specification)
  ) {
    dependent <- as.character(specification$dependent)
    independent <- if ("independent" %in% names(specification)) {
      unique(c(
        unlist(lapply(specification$independent, osem_shiny_spec_split_plus), use.names = FALSE),
        unlist(lapply(specification$independent, osem_shiny_spec_formula_tokens), use.names = FALSE)
      ))
    } else {
      character()
    }
    lag <- if ("lag" %in% names(specification)) {
      unlist(lapply(specification$lag, osem_shiny_spec_split_lag), use.names = FALSE)
    } else {
      character()
    }
    c(dependent, independent, lag)
  } else {
    character()
  }
  variables <- trimws(c(dictionary_variables, specification_variables, include))
  variables <- variables[!is.na(variables) & nzchar(variables)]
  sort(unique(variables))
}

osem_shiny_spec_module_labels <- function(specification) {
  specification <- tryCatch(
    osem_shiny_normalise_specification(specification),
    error = function(e) NULL
  )
  if (is.null(specification) || nrow(specification) == 0L) {
    return(character())
  }

  type_label <- ifelse(
    specification$type == "d",
    "Identity",
    ifelse(specification$type == "n", "Estimated", "Invalid type")
  )
  dependent <- ifelse(
    nzchar(specification$dependent),
    specification$dependent,
    "(unnamed module)"
  )
  cvar_label <- ifelse(
    nzchar(specification$cvar),
    paste0(" | CVAR: ", specification$cvar),
    ""
  )
  labels <- sprintf(
    "%02d | %s | %s%s",
    seq_len(nrow(specification)),
    dependent,
    type_label,
    cvar_label
  )
  stats::setNames(as.character(seq_len(nrow(specification))), labels)
}

osem_shiny_specification_summary <- function(specification) {
  specification <- tryCatch(
    osem_shiny_normalise_specification(specification),
    error = function(e) data.frame(
      type = character(), dependent = character(), independent = character(),
      lag = character(), cvar = character(), stringsAsFactors = FALSE
    )
  )
  lag_terms <- unique(unlist(
    lapply(specification$lag, osem_shiny_spec_split_lag),
    use.names = FALSE
  ))
  cvar_systems <- unique(specification$cvar[nzchar(specification$cvar)])

  list(
    modules = nrow(specification),
    estimated = sum(specification$type == "n"),
    identities = sum(specification$type == "d"),
    cvar_systems = length(cvar_systems),
    lag_only_variables = length(lag_terms)
  )
}

osem_shiny_spec_editor_row <- function(type,
                                       dependent,
                                       independent_variables = character(),
                                       identity_expression = "",
                                       lag_variables = character(),
                                       cvar_system = "") {
  type <- tolower(osem_shiny_spec_scalar(type, "n"))
  if (!type %in% c("n", "d")) type <- "n"
  dependent <- osem_shiny_spec_scalar(dependent, "")

  if (identical(type, "d")) {
    independent <- osem_shiny_spec_scalar(identity_expression, "")
    lag <- ""
    cvar <- ""
  } else {
    independent_variables <- trimws(as.character(independent_variables %||% character()))
    independent_variables <- independent_variables[
      !is.na(independent_variables) & nzchar(independent_variables)
    ]
    independent_variables <- unique(independent_variables)
    independent <- osem_shiny_spec_join_plus(independent_variables)

    lag_variables <- trimws(as.character(lag_variables %||% character()))
    lag_variables <- lag_variables[!is.na(lag_variables) & nzchar(lag_variables)]
    lag_variables <- intersect(unique(lag_variables), independent_variables)

    cvar <- osem_shiny_spec_scalar(cvar_system, "")
    lag <- if (nzchar(cvar)) "" else osem_shiny_spec_join_lag(lag_variables)
  }

  data.frame(
    type = type,
    dependent = dependent,
    independent = independent,
    lag = lag,
    cvar = cvar,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

osem_shiny_spec_apply_editor_row <- function(specification,
                                             index,
                                             row,
                                             synchronise_cvar = TRUE) {
  specification <- osem_shiny_normalise_specification(specification)
  row <- osem_shiny_normalise_specification(row)
  index <- suppressWarnings(as.integer(index))

  if (length(index) != 1L || is.na(index) || index < 1L || index > nrow(specification)) {
    stop("The selected module no longer exists.", call. = FALSE)
  }
  if (nrow(row) != 1L) {
    stop("The guided editor must produce exactly one module row.", call. = FALSE)
  }

  specification[index, ] <- row[1L, names(specification), drop = FALSE]

  if (isTRUE(synchronise_cvar) && nzchar(row$cvar[[1L]])) {
    members <- which(specification$cvar == row$cvar[[1L]])
    specification$type[members] <- "n"
    specification$independent[members] <- row$independent[[1L]]
    specification$lag[members] <- ""
  }

  osem_shiny_normalise_specification(specification)
}

osem_shiny_spec_draft_issues <- function(specification, index, row,
                                           synchronise_cvar = FALSE) {
  candidate <- tryCatch(
    osem_shiny_spec_apply_editor_row(
      specification = specification,
      index = index,
      row = row,
      synchronise_cvar = isTRUE(synchronise_cvar)
    ),
    error = function(e) e
  )
  if (inherits(candidate, "error")) {
    return(osem_shiny_add_issue(
      osem_shiny_empty_issues(),
      "error",
      "Specification",
      conditionMessage(candidate)
    ))
  }
  osem_shiny_validate_specification(candidate)$issues
}

osem_shiny_spec_graph_data <- function(specification) {
  specification <- tryCatch(
    osem_shiny_normalise_specification(specification),
    error = function(e) NULL
  )
  empty_vertices <- data.frame(
    name = character(), module_type = character(), cvar = character(),
    stringsAsFactors = FALSE
  )
  empty_edges <- data.frame(
    from = character(), to = character(), edge_type = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(specification) || nrow(specification) == 0L) {
    return(list(vertices = empty_vertices, edges = empty_edges))
  }

  edge_rows <- list()
  edge_index <- 0L
  for (i in seq_len(nrow(specification))) {
    dependent <- specification$dependent[[i]]
    if (!nzchar(dependent)) next

    rhs <- if (identical(specification$type[[i]], "n")) {
      osem_shiny_spec_split_plus(specification$independent[[i]])
    } else {
      osem_shiny_spec_formula_tokens(specification$independent[[i]])
    }
    lag_variables <- osem_shiny_spec_split_lag(specification$lag[[i]])
    if (length(rhs) == 0L) next

    for (variable in rhs) {
      edge_index <- edge_index + 1L
      edge_rows[[edge_index]] <- data.frame(
        from = variable,
        to = dependent,
        edge_type = if (
          identical(specification$type[[i]], "d")
        ) {
          "identity"
        } else if (variable %in% lag_variables) {
          "lag-only"
        } else {
          "contemporaneous"
        },
        stringsAsFactors = FALSE
      )
    }
  }

  edges <- if (length(edge_rows) > 0L) {
    unique(do.call(rbind, edge_rows))
  } else {
    empty_edges
  }

  variables <- unique(c(
    specification$dependent[nzchar(specification$dependent)],
    edges$from,
    edges$to
  ))
  variables <- variables[!is.na(variables) & nzchar(variables)]
  vertices <- data.frame(
    name = variables,
    module_type = "Exogenous",
    cvar = "",
    stringsAsFactors = FALSE
  )

  dependent_match <- match(vertices$name, specification$dependent)
  is_dependent <- !is.na(dependent_match)
  vertices$module_type[is_dependent] <- ifelse(
    specification$type[dependent_match[is_dependent]] == "d",
    "Identity",
    "Estimated"
  )
  vertices$cvar[is_dependent] <- specification$cvar[dependent_match[is_dependent]]

  list(vertices = vertices, edges = edges)
}

osem_shiny_plot_specification_graph <- function(specification) {
  graph_data <- osem_shiny_spec_graph_data(specification)
  if (nrow(graph_data$vertices) == 0L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Add a module to display the dependency graph.")
    return(invisible(NULL))
  }

  graph <- igraph::graph_from_data_frame(
    d = graph_data$edges,
    directed = TRUE,
    vertices = graph_data$vertices
  )

  layout <- if (igraph::ecount(graph) == 0L) {
    igraph::layout_in_circle(graph)
  } else {
    tryCatch(
      igraph::layout_with_sugiyama(graph)$layout,
      error = function(e) igraph::layout_nicely(graph)
    )
  }

  module_type <- igraph::vertex_attr(graph, "module_type")
  fill <- c(
    "Estimated" = "#2f73b8",
    "Identity" = "#dceaf7",
    "Exogenous" = "#edf1f6"
  )[module_type]
  label_colour <- ifelse(module_type == "Estimated", "white", "#08265a")
  shape <- ifelse(module_type == "Exogenous", "square", "circle")
  cvar <- igraph::vertex_attr(graph, "cvar")
  labels <- igraph::V(graph)$name
  labels[nzchar(cvar)] <- paste0(labels[nzchar(cvar)], "\n[", cvar[nzchar(cvar)], "]")

  edge_type <- if (igraph::ecount(graph) > 0L) {
    igraph::edge_attr(graph, "edge_type")
  } else {
    character()
  }
  edge_lty <- ifelse(edge_type == "lag-only", 2, 1)
  edge_colour <- ifelse(edge_type == "identity", "#7b8794", "#315e8a")

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))

  igraph::plot.igraph(
    graph,
    layout = layout,
    vertex.color = unname(fill),
    vertex.frame.color = "#6d7f91",
    vertex.label = labels,
    vertex.label.color = label_colour,
    vertex.label.cex = 0.82,
    vertex.size = 29,
    vertex.size2 = 23,
    vertex.shape = shape,
    edge.color = edge_colour,
    edge.lty = edge_lty,
    edge.arrow.size = 0.35,
    edge.curved = 0.08,
    asp = 0.85,
    margin = 0.08
  )

  graphics::legend(
    "topleft",
    legend = c("Estimated", "Identity", "Exogenous", "Lag-only edge"),
    pch = c(21, 21, 22, NA),
    pt.bg = c("#2f73b8", "#dceaf7", "#edf1f6", NA),
    col = c("#6d7f91", "#6d7f91", "#6d7f91", "#315e8a"),
    lty = c(NA, NA, NA, 2),
    bty = "n",
    cex = 0.8
  )

  invisible(graph_data)
}
