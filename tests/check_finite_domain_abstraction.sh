#!/bin/sh
set -eu

build_dir=${1:?usage: check_finite_domain_abstraction.sh BUILD_DIR}
tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/assemble-domain-abstraction.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT HUP INT TERM

cat >"$tmp_dir/valid.ml" <<'EOF'
let decode_zero domain = Finite_domain.decode domain 0
EOF

cat >"$tmp_dir/forge_identity.ml" <<'EOF'
let forge (domain : int Finite_domain.t) =
  { domain with identity = ref () }
EOF

cat >"$tmp_dir/raw_decode.ml" <<'EOF'
let bypass (domain : int Finite_domain.t) =
  domain.decode (-1)
EOF

ocamlc -I "$build_dir" -c "$tmp_dir/valid.ml"

if ocamlc -I "$build_dir" -c "$tmp_dir/forge_identity.ml" \
     >"$tmp_dir/forge.out" 2>&1; then
  echo "finite-domain abstraction: forged identity unexpectedly compiled" >&2
  exit 1
fi

if ocamlc -I "$build_dir" -c "$tmp_dir/raw_decode.ml" \
     >"$tmp_dir/decode.out" 2>&1; then
  echo "finite-domain abstraction: raw decoder access unexpectedly compiled" >&2
  exit 1
fi

echo "finite-domain abstraction rejects identity forgery and raw decoder access PASS"
