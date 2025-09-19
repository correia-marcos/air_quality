#!/usr/bin/env bash
set -euo pipefail

USER_NAME="${USER:-rstudio}"
PASS="${PASSWORD:-secret123}"

# The rocker/rstudio image already has 'rstudio' user; keep this idempotent
if ! id -u "$USER_NAME" >/dev/null 2>&1; then
  useradd -m -s /bin/bash "$USER_NAME" || true
  usermod -aG staff "$USER_NAME" || true
fi
echo "${USER_NAME}:${PASS}" | chpasswd

mkdir -p /usr/local/lib/R/renv-cache
chown -R "${USER_NAME}":staff /usr/local/lib/R/renv-cache

echo "Entrypoint args: $*"
echo "R: $(which R) | Rscript: $(which Rscript)"
R -q -e "cat('R_LIBS_USER=', Sys.getenv('R_LIBS_USER'), '\n', sep='')"

# 1) Batch runner: run a sequence of R scripts
if [ "${1:-}" = "run" ]; then
  shift
  if [ "$#" -eq 0 ]; then
    echo "Error: No R script specified after 'run'."; exit 1
  fi
  for script in "$@"; do
    echo "Running R script: ${script}"
    Rscript "${script}"
  done
  exit 0
fi

# 2) Shell
if [ "${1:-}" = "bash" ]; then
  echo "Dropping to bash shell..."
  exec bash
fi

# 3) RStudio Server
if [ "$#" -eq 0 ] || [ "${1:-}" = "rserver" ]; then
  echo "Starting RStudio Server in foreground..."
  exec rserver \
    --www-port=8787 \
    --server-user="${USER_NAME}" \
    --auth-none=0 \
    --auth-timeout-minutes=5 \
    --rsession-which-r="${RSESSION_WHICH_R:-/usr/local/bin/R}" \
    --server-daemonize=0
fi

# 4) Fallback
echo "Executing custom command: $*"
exec "$@"