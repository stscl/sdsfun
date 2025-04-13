#ifndef DummyVar_H
#define DummyVar_H

#include "SDSUtils.h"
#include <RcppArmadillo.h>

Rcpp::IntegerMatrix DummyVar(Rcpp::IntegerVector x);
Rcpp::IntegerMatrix DummyMat(Rcpp::IntegerMatrix mat);

#endif // DummyVar_H
