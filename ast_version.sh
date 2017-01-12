#!/usr/bin/env bash
usage()
{
  cat 2>&1 <<EOF
Usage: $0 <ocaml compiler command>"
Returns the version (as 402, 403, 404, 405) of the parsetree used by this version of OCaml.
EOF
  exit 1
}

if [ -z "$1" ]; then
  usage 
fi 
trap usage ERR

VAR=$(exec "$@" -config | grep '^version:' | cut -f 2 -d ' ')
case "$VAR" in
  4.02.*)
    VERSION=402
  ;;
  4.03.*)
    VERSION=403
  ;;
  4.04.*)
    VERSION=404
  ;;
  4.05.*)
    VERSION=405
  ;;
  *)
    printf "Unkown OCaml version %s\n" "$VAR" 1>&2
    exit 1
  ;;
esac  

printf "%s" "$VERSION"
