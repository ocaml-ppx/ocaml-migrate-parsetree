#!/bin/sh

set -e

OCAML_VERSION="$1"
FNAME="$2"

# Note: the spaces in the replacements are to preserve locations
SED_VERSION="s/OCAML_VERSION/$OCAML_VERSION      /g"
SED_IF_CURRENT="s/(\*IF_CURRENT \([^\*]*\)\*)/             \1/"

echo "# 1 \"$FNAME\""
if [ `basename "$FNAME"` = ast_$OCAML_VERSION.ml ]; then
    exec sed -e "$SED_VERSION" -e "$SED_IF_CURRENT" "$FNAME"
else
    exec sed -e "$SED_VERSION" "$FNAME"
fi
