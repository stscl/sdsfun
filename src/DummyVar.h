#ifndef DummyVar_H
#define DummyVar_H

#include <Rcpp.h>

Rcpp::IntegerVector Runique(Rcpp::IntegerVector x);
Rcpp::IntegerMatrix DummyVar(Rcpp::IntegerVector x);
Rcpp::IntegerMatrix DummyMat(Rcpp::IntegerMatrix mat);

#endif // DummyVar_H
