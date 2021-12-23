FROM ocaml/opam:ubuntu-20.04-ocaml-4.12
RUN sudo apt-get update && sudo apt-get install -y libev-dev pkg-config libssl-dev
RUN opam init
RUN opam switch create 4.12.0
RUN opam install core yojson bisect_ppx ppx_jane ppx_deriving_yojson dream lwt_ppx ounit2
USER root
COPY api /api
RUN chown opam /api
USER opam
WORKDIR /api
EXPOSE 8080
ENTRYPOINT eval $(opam env) && dune exec src/server.exe