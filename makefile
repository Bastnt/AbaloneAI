ocamlc -c LegalMoves.ml AlphaBeta.ml  InputOutput.ml
ocamlc LegalMoves.cmo AlphaBeta.cmo InputOutput.cmo -o abalone
