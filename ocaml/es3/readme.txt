ocamlc -c Matrix.mli
ocamlc -c Matrix.ml
ocamlc -c main.ml
ocamlc -o main Matrix.cmo main.cmo