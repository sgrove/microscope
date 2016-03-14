#!/usr/bin/env sh

set -e

pkill main.native || true
echo "Rebuilding main"
ocamlbuild -use-ocamlfind -tag thread -pkgs websocket.lwt,core,opium,cohttp.lwt,irmin.unix,irmin,lwt.ppx,yojson,ppx_deriving_yojson -I src/ main.native
echo "Running server"
./main.native &
echo "Done"
