# Minimal launcher. All application logic lives in R/osem_shiny_*.R.
if (!requireNamespace("osem", quietly = TRUE)) {
  stop("Install or load the 'osem' package before launching this app.", call. = FALSE)
}

osem:::osem_shiny_create_app()
