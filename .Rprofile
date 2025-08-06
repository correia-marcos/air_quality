# load the project’s renv
source("renv/activate.R")

if (interactive() && identical(Sys.getenv("IN_DOCKER"), "true")) {
  setwd("/air_monitoring")                  # ← first switch into your project
  source("renv/activate.R")                 # ← then source the project’s activate script
  if (requireNamespace("renv", quietly = TRUE))
    renv::activate()                        # ← finally hook up renv
}
