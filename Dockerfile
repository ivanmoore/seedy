FROM haskell:7.10.3
WORKDIR .
ADD . /seedy
WORKDIR seedy
RUN ./build.sh
CMD ["./run-tests.sh"]
