#!/usr/bin/env bash
set -e

# --- ensure 'rstudio' user exists and set password from env ---
USER_NAME="${USER:-rstudio}"
PASS="${PASSWORD:-secret123}"

if ! id -u "$USER_NAME" >/dev/null 2>&1; then
  useradd -m -s /bin/bash "$USER_NAME"
  usermod -aG staff "$USER_NAME" || true
fi

echo "${USER_NAME}:${PASS}" | chpasswd

# --- ensure renv cache dir exists and is writable ---
mkdir -p /usr/local/lib/R/renv-cache
chown -R "${USER_NAME}":staff /usr/local/lib/R/renv-cache

echo "Entrypoint script started with args: $@"

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
  echo "Dropping to bash shell..."
  exec bash
fi

# 3) If no arguments (or 'rserver'), start RStudio Server
if [ "$#" -eq 0 ] || [ "$1" = "rserver" ]; then
  echo "Starting RStudio Server in foreground..."
  exec rserver \
    --www-port=8787 \
    --server-user="${USER_NAME}" \
    --auth-none=0 \
    --auth-timeout-minutes=5 \
    --rsession-which-r=/usr/local/bin/R \
    --server-daemonize=0
fi

# 4) Otherwise, exec whatever was passed
echo "Executing custom command: $@"
exec "$@"