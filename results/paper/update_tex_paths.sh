#!/bin/sh
# Rewrite a manuscript's \includegraphics paths for the results/paper/figures tree.
# Every replacement is read from tex_path_mapping.csv, so the two cannot drift apart.
# Usage: sh update_tex_paths.sh <your.tex>
set -e

TEX="$1"
MAP="$(dirname "$0")/tex_path_mapping.csv"

if [ ! -f "$TEX" ]; then echo "No such file: $TEX" >&2; exit 1; fi
if [ ! -f "$MAP" ]; then echo "No such file: $MAP" >&2; exit 1; fi

# Turn each "old","new" row into one s|old|new|g line of a sed script. Paths hold no
# "|", so it is a safe delimiter. The header row is skipped.
SCRIPT=$(mktemp)
trap 'rm -f "$SCRIPT"' EXIT

tail -n +2 "$MAP" \
  | sed 's/^"//; s/"$//; s/","/|/' \
  | awk -F'|' 'NF == 2 { print "s|" $1 "|" $2 "|g" }' > "$SCRIPT"

sed -i.bak -f "$SCRIPT" "$TEX"

echo "Rewrote $TEX with $(wc -l < "$SCRIPT" | tr -d ' ') rules (backup at $TEX.bak)"
