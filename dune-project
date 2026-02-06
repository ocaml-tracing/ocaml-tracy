(lang dune 2.7)

(generate_opam_files true)

(version 0.7)

(name tracy-client)

(homepage "https://github.com/ocaml-tracing/ocaml-tracy")

(bug_reports "https://github.com/ocaml-tracing/ocaml-tracy/issues")

(source
 (github "ocaml-tracing/ocaml-tracy"))

(package
 (name tracy-client)
 (license BSD-3-Clause)
 (maintainers "Simon Cruanes")
 (authors "Bartosz Taudul" "Simon Cruanes")
 (depends
  (ocaml
   (>= 4.08))
  (trace
   (and
    (>= 0.11)
    (< 0.12))))
 (synopsis "Client bindings to the Tracy profiler (v0.13)"))
