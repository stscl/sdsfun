#ifndef SDSUtils_H
#define SDSUtils_H

// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

std::string SignificanceStar(double p);
Rcpp::IntegerVector RcppUnique(Rcpp::IntegerVector x);

#endif // SDSUtils_H
