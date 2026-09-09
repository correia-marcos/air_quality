# Isolate one existing stage, record its outcome, and propagate failure to make.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[1])) stop("Supply one existing stage script")
script <- normalizePath(args[1])
run <- Sys.getenv("AIR_VERIFY_RUN")
if (!nzchar(run)) stop("AIR_VERIFY_RUN is required for stage recording")
dir.create(file.path(run, "stages"), recursive = TRUE, showWarnings = FALSE)
set.seed(20230901)
started <- Sys.time()
record <- list(script = args[1], script_sha256 = digest::digest(file = script, algo = "sha256"),
               parameters = "Stage defaults as recorded in the hashed script and code inventory",
               seed = 20230901, started_utc = format(started, tz = "UTC", usetz = TRUE),
               warnings = character(), exit_status = 0L)
tryCatch(withCallingHandlers(source(script, local = new.env(parent = globalenv())),
  warning = function(w) record$warnings <<- c(record$warnings, conditionMessage(w))),
  error = function(e) {
    record$exit_status <<- 1L
    record$error <<- conditionMessage(e)
    message("Stage failed: ", conditionMessage(e))
  })
record$seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
jsonlite::write_json(record, file.path(run, "stages", paste0(basename(script), ".json")),
                     pretty = TRUE, auto_unbox = TRUE)
quit(status = record$exit_status)
