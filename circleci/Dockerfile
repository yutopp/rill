FROM base/archlinux
MAINTAINER yutopp

RUN pacman -Syu --noconfirm
RUN pacman-db-upgrade
RUN pacman -S --noconfirm clang boost llvm
RUN pacman -S --noconfirm cmake
RUN pacman -S --noconfirm autoconf automake make ruby