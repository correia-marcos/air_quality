# 1. Detect Docker & Switch Directory FIRST
#    We must do this before trying to load renv, because R starts in /home/rstudio
if (identical(Sys.getenv("IN_DOCKER"), "true")) {
  if (dir.exists("/air_monitoring")) {
    setwd("/air_monitoring")
  }
}

# 2. NOW source renv
#    Since we are now inside /air_monitoring, this relative path will work.
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# 3. Optional: Auto-activate for interactive sessions
#    (renv usually handles this via activate.R, but this ensures it)
if (interactive() && requireNamespace("renv", quietly = TRUE)) {
  renv::activate()
}