#!/bin/sh

set -e

OCAML_VERSION="$1"
FNAME="$2"

# Note: the spaces in the replacements are to preserve locations

echo "# 1 \"$FNAME\""
if [ `basename "$FNAME"` = ast_$OCAML_VERSION.ml ]; then
    exec tools/pp_subst.native -DOCAML_VERSION="$OCAML_VERSION" -DCURRENT "$FNAME"
else
    exec tools/pp_subst.native -DOCAML_VERSION="$OCAML_VERSION" "$FNAME"
fi
