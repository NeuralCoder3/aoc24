FROM ubuntu:20.04

RUN apt update && \
    DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin apt install -y \
    libssl-dev build-essential curl pkg-config sudo unzip git tzdata \
    z3 \
    python3 python3-pip python3-venv python-is-python3 \
    opam libgmp-dev

RUN opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam switch create main --packages=ocaml-base-compiler.4.14.2 && \
    echo 'eval $(opam env --switch=main)' >> ~/.bashrc && \
    . ~/.bashrc && \
    opam install -y \
        opam-depext ocaml-secondary-compiler ocaml-lsp-server ocamlformat merlin dune user-setup \
        base batteries zarith \
        z3 ppx_base ppx_deriving && \
    opam user-setup install

# other helpful tools (typst, latex)
RUN apt install -y texlive-latex-extra latexmk chktex texlive-extra-utils && \
    echo "PATH=$PATH:$HOME/.cargo/bin" >> ~/.bashrc && \
    . ~/.bashrc && \
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y && \
    cd ~ && \
    cargo init && \
    cargo install typst-cli just


# graphics and debugging tools
# everything except cairo is for GUI building
# FYI: we somehow need to activate opam twice
RUN apt-get update && \
    apt install -y software-properties-common&& \
    add-apt-repository -y ppa:savoury1/multimedia && \
    apt-get update && \
    apt install -y gdb libcairo2-dev libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev && \
    opam install -y cairo2 bogue && \
    echo 'eval $(opam env --switch=main)' >> ~/.bashrc