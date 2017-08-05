FROM haskell:7.10.3
ADD install-dependencies.sh .
RUN ./install-dependencies.sh