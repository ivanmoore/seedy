#!/bin/bash

apt-get update
apt-get -y install git
apt-get install curl
curl -sSL https://get.haskellstack.org/ | sh
PATH="$PATH:~/.local/bin"
git clone https://github.com/ivanmoore/seedy.git
cd seedy
stack install --install-ghc

# This installs the seedy-exe executable in /root/.local/bin
# It can be run simply by typing "seedy-exe" anywhere on the machine.

# I don't know how to docker. I leave this to you dad, you can explain it to me on monday
