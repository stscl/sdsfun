#include <Rcpp.h>
using namespace Rcpp;

double RcppMatSum(Rcpp::NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  double out = 0;
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      out += mat(i, j);
    }
  }
  return out;
}

// [[Rcpp::export]]
double RcppSpatialVariance(Rcpp::NumericVector x, Rcpp::NumericMatrix wt) {
  int n = x.size();
  double out = 0;
  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < n; ++j) {
      double w = wt(i, j);
      out += w * pow((x[i]-x[j]),2) / 2;
    }
  }
  out = out / RcppMatSum(wt);
  return out;
}
