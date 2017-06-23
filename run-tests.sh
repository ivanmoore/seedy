#!/bin/bash
cd test
runghc -i../src/ -i../lib/ EndToEndTest.hs
cd ..
