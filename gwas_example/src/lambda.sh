#!/bin/bash
# Load required modules
# This are for fidis
module load intel/18.0.5 intel-mkl/2018.5.274 r/3.6.0 &&\

Rscript lambda.R

See Rscript for method export the pvalues from analysis output
