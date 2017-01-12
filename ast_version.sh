#!/usr/bin/env bash
usage()
{
  cat 2>&1 <<EOF
Usage: $0 <ocaml compiler command>"
Returns the version (as 402, 403 or 404) of the parsetree used by this version of OCaml.
EOF
  exit 1
}

if [ -z "$1" ]; then
  usage 
fi 
trap usage ERR

VAR=$(exec "$@" -config | grep cmi_magic_number | cut -f 2 -d ' ')
case "$VAR" in
  Caml1999I017)
    VERSION=402
  ;;
  Caml1999I020)
    VERSION=403
  ;;
  Caml1999I021)
    VERSION=404
  ;;
  *)
    exit 1
  ;;
esac  

printf "%s" "$VERSION"
