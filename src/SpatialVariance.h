#ifndef SpatialVariance_H
#define SpatialVariance_H

#include <cmath>
#include <RcppArmadillo.h>

double RcppSpatialVariance(Rcpp::NumericVector x, Rcpp::NumericMatrix wt);

#endif // SpatialVariance_H
