# entrypoint.sh (UPDATED)
#!/usr/bin/env bash
set -e

# 1) If called as 'run', execute R scripts in order
if [ "$1" = "run" ]; then
  shift
  if [ "$#" -eq 0 ]; then
    echo "Error: No R script specified after 'run'."
    exit 1
  fi
  for script in "$@"; do
    echo "Running R script: ${script}"
    Rscript "${script}"
  done
  exit 0
fi

# 2) If called as 'bash', drop to shell
if [ "$1" = "bash" ]; then
  exec bash
fi

# 3) If no arguments (or 'rserver'), start RStudio Server
if [ "$#" -eq 0 ] || [ "$1" = "rserver" ]; then
  exec rserver --server-daemonize=0 --www-port=8787
fi

# 4) Otherwise, exec whatever was passed
exec "$@"