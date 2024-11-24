#include <Rcpp.h>
#include "SDSUtils.h"
#include "SpatialVariance.h"
using namespace Rcpp;

// [[Rcpp::export]]
double CalcSPADEPSD(Rcpp::NumericVector y,
                    Rcpp::IntegerVector h,
                    Rcpp::NumericMatrix wt) {
  int N = y.size();

  // Calculate the global variance using spatial variance
  double denom = RcppSpatialVariance(y, wt) * y.size();

  // Get unique levels of h
  Rcpp::IntegerVector unique_levels = RcppUnique(h);

  // Calculate the numerator (sum of variances within each level)
  double numer = 0;
  for (int level : unique_levels) {
    // Get indices corresponding to the current level
    Rcpp::IntegerVector level_indices;
    for (int i = 0; i < N; ++i) {
      if (h[i] == level) {
        level_indices.push_back(i);
      }
    }

    // Subset y and wt based on the indices for the current level
    Rcpp::NumericVector y_h = y[level_indices];
    Rcpp::NumericMatrix wt_h(level_indices.size(), level_indices.size());

    for (int i = 0; i < level_indices.size(); ++i) {
      for (int j = 0; j < level_indices.size(); ++j) {
        // Select the corresponding entries from wt for this subset
        wt_h(i, j) = wt(level_indices[i], level_indices[j]);
      }
    }

    // Calculate the spatial variance for the current level
    double level_variance = RcppSpatialVariance(y_h, wt_h);
    numer += level_variance * y_h.size();  // Variance multiplied by the sample size of the level
  }

  // Calculate the SPADPSD value
  double spade_psd = 1 - (numer / denom);

  return spade_psd;
}
