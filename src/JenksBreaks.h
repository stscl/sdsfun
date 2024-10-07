#ifndef JenksBreaks_H
#define JenksBreaks_H

#include <Rcpp.h>

Rcpp::NumericVector RcppJenksBreaks(const Rcpp::NumericVector& x,
                                    int n_classes, bool is_sorted = false);

#endif // JenksBreaks_H
