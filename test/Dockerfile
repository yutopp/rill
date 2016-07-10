FROM base/archlinux

MAINTAINER yutopp

# system
RUN pacman -Sy --noconfirm archlinux-keyring
RUN pacman -S --needed --noconfirm base-devel
RUN pacman-db-upgrade
RUN pacman -S --noconfirm git wget unzip

# get ocaml and opam
RUN pacman -S --noconfirm ocaml
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
RUN pacman -S --noconfirm ncurses && ln -s /usr/lib/libncursesw.so.6 /usr/lib/libncursesw.so.5  # workaround...
RUN pacman -S --noconfirm libunistring

# packages
RUN opam init
RUN pacman -S --noconfirm llvm llvm-libs
RUN pacman -S --noconfirm python
RUN opam install omake menhir batteries ctypes-foreign stdint llvm.3.8
RUN opam install bisect_ppx # for testing

#
RUN mkdir /cibase
WORKDIR /cibase