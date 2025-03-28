#!/bin/bash
set -e

# Check if an argument was provided
if [ "$#" -eq 0 ]; then
  # No arguments: drop into an interactive shell
  exec /bin/bash
fi

# If the first argument is "run", shift it off and run the provided R script(s)
if [ "$1" = "run" ]; then
  shift
  if [ "$#" -eq 0 ]; then
    echo "Error: No R script specified after 'run'."
    exit 1
  fi
  # Run each provided R script in order
  for script in "$@"; do
    echo "Running R script: ${script}"
    Rscript "${script}"
  done
  exit 0
fi

# If the first argument is "bash", start an interactive shell
if [ "$1" = "bash" ]; then
  exec /bin/bash
fi

# Default: execute any command passed as arguments
exec "$@"
