#!/bin/bash

cd third-party
git clone https://bitbucket.org/jshs/monpoly.git
cd monpoly
git checkout d0dc92d40ecab5fe088470125a0681a010e2a12b
cd ..
git clone https://github.com/leonardolima/dejavu-online.git
git clone https://git.ku.dk/kfx532/timelymon.git
cd timelymon
git checkout RV24_Tool_Paper 
cd ..



