RESULT = server
SOURCES = \
  color.mli \
  color.ml \
  move.mli \
  move.ml \
  board.mli \
  board.ml \
  request.mll \
  player.mli \
  player.ml \
  server.ml
LIBS = unix

include OCamlMakefile
