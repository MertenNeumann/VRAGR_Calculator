# Run from the repository root; dependencies can be restored with scripts/setup.R.
if (!file.exists("app.R")) stop("Run from the repository root.")
port <- suppressWarnings(as.integer(Sys.getenv("VRAGR_PORT", "3838")))
if (is.na(port) || port < 1024L || port > 65535L) stop("VRAGR_PORT must be between 1024 and 65535.")
shiny::runApp(".", host="127.0.0.1", port=port, launch.browser=FALSE)
