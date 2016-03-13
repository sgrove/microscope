#!/usr/bin/env bash
echo "Rebuilding bytecode"
ocamlfind ocamlc -g -package js_of_ocaml -package js_of_ocaml.syntax -package js_of_ocaml.ppx -linkpkg -o client.byte src/client/main.ml
echo "Outputting js"
js_of_ocaml --source-map --pretty --no-inline --debug-info -o resources/public/js/client.js client.byte
echo "Done"
