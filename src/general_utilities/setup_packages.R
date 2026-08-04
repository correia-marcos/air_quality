# ============================================================================================
# IDB: Air monitoring — package setup mechanism
# ============================================================================================
# @Goal: One implementation of "check the project library, then attach".
#
# @Description: Sourced by each config_utils_<stage>.R, which then calls both functions on its
#   own `pkgs` vector. The mechanism is shared; the package LIST is not — each stage declares
#   its own, because that list is the stage's dependency declaration and belongs next to it.
#   Defines functions only, so it is safe to source more than once.
#
# @Summary:
#   I.  ensure_installed — fail loudly, or install, when the project library is incomplete
#   II. attach_packages  — attach quietly
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: ensure_installed
#
# @Arg       : pkgs — character vector of package names the stage needs.
#
# @Output    : invisible NULL. Installs anything missing from the project library.
#
# @Purpose   : A referee restoring the package with renv should get a clear message naming what
#              is missing rather than a "there is no package called X" deep inside a function.
#
# @Details   : rnaturalearthhires is not on CRAN, so it comes from the rOpenSci r-universe. That
#              needs a different `repos`, which is restored on exit — leaving it set would send
#              every later install in the session to r-universe.
#
# @Written_by: Marcos Paulo
# @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
ensure_installed <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (!length(miss)) {
    return(invisible(NULL))
  }

  message(
    "Missing packages: ", paste(miss, collapse = ", "),
    ". Run renv::restore() (or install locally with renv::install() then renv::snapshot())."
  )

  old_repos <- getOption("repos")
  on.exit(options(repos = old_repos), add = TRUE)

  if ("rnaturalearthhires" %in% miss) {
    options(repos = c(CRAN = "https://ropensci.r-universe.dev"))
    renv::install("rnaturalearthhires")
    miss <- miss[miss != "rnaturalearthhires"]
  }

  if (length(miss)) {
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
    renv::install(miss)
  }

  invisible(NULL)
}


# --------------------------------------------------------------------------------------------
# Function: attach_packages
#
# @Arg       : pkgs — character vector of package names to attach.
#
# @Output    : invisible NULL.
#
# @Purpose   : Attaches the stage's packages without the startup banners, so a script's console
#              output starts with the pipeline's own messages.
#
# @Written_by: Marcos Paulo
# @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
attach_packages <- function(pkgs) {
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}
