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
RUN pacman -S --noconfirm llvm llvm-libs llvm-ocaml
RUN ln -s /usr/lib/libLLVM.so.3.7 /usr/lib/libLLVM-3.7.so   # workaround...
RUN pacman -S --noconfirm python
RUN opam install omake menhir batteries ctypes-foreign llvm.3.7

#
RUN mkdir /cibase
WORKDIR /cibase