#ifndef JenksBreaks_H
#define JenksBreaks_H

#include <Rcpp.h>

Rcpp::NumericVector GetJenksBreaks(Rcpp::NumericVector inp_data,
                                   int n_classes,bool is_sorted = true);

#endif // JenksBreaks_H
