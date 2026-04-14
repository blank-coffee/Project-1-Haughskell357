#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-}"
REMOVE="${2:-}"

if [ -z "$ROOT" ]; then
    echo "Usage: dedupe.sh <root-folder> [--remove]"
    exit 1
fi

DELETEME="$ROOT/deleteme"
mkdir -p "$DELETEME"

# macOS uses shasum, Linux uses sha256sum
if command -v sha256sum >/dev/null 2>&1; then
    HASH_CMD="sha256sum"
else
    HASH_CMD="shasum -a 256"
fi

# Collect all files except those already in deleteme
mapfile -t FILES < <(find "$ROOT" -type f ! -path "*/deleteme/*")

declare -A GROUPS

echo "Scanning files..."
for f in "${FILES[@]}"; do
    HASH=$($HASH_CMD "$f" | awk '{print $1}')
    GROUPS["$HASH"]+="$f;"
done

echo "Processing duplicates..."
for hash in "${!GROUPS[@]}"; do
    IFS=';' read -ra FILEGROUP <<< "${GROUPS[$hash]}"

    # Only process groups with >1 file
    if [ "${#FILEGROUP[@]}" -le 1 ]; then
        continue
    fi

    # Sort by modification time (oldest first)
    IFS=$'\n' FILEGROUP_SORTED=($(ls -1t "${FILEGROUP[@]}" | tac))

    KEEPER="${FILEGROUP_SORTED[0]}"
    echo "Keeper: $KEEPER"

    for ((i=1; i<${#FILEGROUP_SORTED[@]}; i++)); do
        SRC="${FILEGROUP_SORTED[$i]}"
        BASENAME=$(basename "$SRC")
        DEST="$DELETEME/$BASENAME"

        # Avoid collisions
        n=1
        while [ -e "$DEST" ]; do
            DEST="$DELETEME/${BASENAME%.*}-$n.${BASENAME##*.}"
            n=$((n+1))
        done

        echo "Moving duplicate: $SRC -> $DEST"

        # Try rename first
        if mv "$SRC" "$DEST" 2>/dev/null; then
            continue
        fi

        # Fallback: copy + remove
        cp "$SRC" "$DEST"
        if [ "$REMOVE" = "--remove" ]; then
            rm "$SRC"
        fi
    done
done

echo "Done."