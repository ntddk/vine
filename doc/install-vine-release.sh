#!/bin/bash
# Instructions for installing Vine release 1.0 on Ubuntu 9.04 Linux 32-bit

# Commands that require root access are preceded with "sudo".

# The prerequisite packages are about 300MB of downloads, and require
# 1.1GB once installed; Vine itself requires about 320MB.

# Last tested 2009-08-17

# This script will build Vine in a "$HOME/bitblaze" directory,
# assuming that vine-1.0.tar.gz is in /tmp.
cd ~
mkdir bitblaze
cd bitblaze

# Prerequisite packages:

# For compiling C++ code:
sudo apt-get install g++

# For OCaml support:
sudo apt-get install ocaml ocaml-findlib libgdome2-ocaml-dev camlidl \
                     libextlib-ocaml-dev ocaml-native-compilers

# Ocamlgraph >= 0.99c is required; luckily the version in Ubuntu 9.04
# is now new enough.
sudo apt-get install libocamlgraph-ocaml-dev

# For the BFD library:
sudo apt-get install binutils-dev

# For building documentation:
sudo apt-get install texlive texlive-latex-extra transfig hevea

# Vine itself:
tar xvzf /tmp/vine-1.0.tar.gz
(cd vine-1.0 && ./configure)
(cd vine-1.0 && make)
(cd vine-1.0/doc/howto && make doc)

