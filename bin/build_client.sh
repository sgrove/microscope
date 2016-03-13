#!/usr/bin/env bash
echo "Rebuilding bytecode"
ocamlfind ocamlc -thread -g -package js_of_ocaml -package js_of_ocaml.syntax -package js_of_ocaml.ppx -package core -package yojson -package ppx_deriving_yojson -linkpkg -o client.byte src/client/main.ml
echo "Outputting js"
js_of_ocaml --disable genprim --source-map --pretty --no-inline --debug-info -o resources/public/js/client.js +weak.js +toplevel.js client.byte
echo "Done"
