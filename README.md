# Getting started

```shell
eval $(opam env)

ocamlfind ocamlopt -package lwt,lwt.unix,logs,logs.lwt -linkpkg -thread -o loop ./bin/main.ml && ./loop 5
```
