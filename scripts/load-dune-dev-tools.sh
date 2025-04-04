
OCAMLLSP="$(dune tools which ocamllsp)"
OCAMLFORMAT="$(dune tools which ocamlformat)"

dune build "${OCAMLLSP}"
dune build "${OCAMLFORMAT}"

export PATH="$PWD/$(dirname "${OCAMLLSP}"):$PATH"
export PATH="$PWD/$(dirname "${OCAMLFORMAT}"):$PATH"

