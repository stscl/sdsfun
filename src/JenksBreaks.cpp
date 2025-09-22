#include <vector>
#include <limits>
#include <algorithm>

// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector RcppJenksBreaks(const Rcpp::NumericVector& x,
                                    int n_classes, bool is_sorted = false) {
  Rcpp::NumericVector inp_data;  // No copy initially

  if (!is_sorted) {
    inp_data = Rcpp::clone(x);  // Only copy and sort if needed
    std::sort(inp_data.begin(), inp_data.end());
  } else {
    inp_data = x;  // Use input directly if already sorted
  }

  int data_length = inp_data.size();

  // Initialize matrices for lower class limits and variance combinations
  Rcpp::IntegerMatrix lower_class_limits(data_length + 1, n_classes + 1);
  Rcpp::NumericMatrix variance_combinations(data_length + 1, n_classes + 1);

  for (int i = 1; i <= n_classes; ++i) {
    lower_class_limits(1, i) = 1;
    variance_combinations(1, i) = 0.0;
    for (int j = 2; j <= data_length; ++j) {
      variance_combinations(j, i) = std::numeric_limits<double>::infinity();
    }
  }

  for (int l = 2; l <= data_length; ++l) {
    double sum = 0.0, sum_squares = 0.0, variance = 0.0;

    for (int m = 1; m <= l; ++m) {
      int lower_class_limit = l - m + 1;
      double val = inp_data[lower_class_limit - 1];
      sum += val;
      sum_squares += val * val;

      // Compute variance at this point
      variance = sum_squares - (sum * sum) / m;
      int i4 = lower_class_limit - 1;
      if (i4 != 0) {
        for (int j = 2; j <= n_classes; ++j) {
          if (variance_combinations(l, j) >= (variance + variance_combinations(i4, j - 1))) {
            lower_class_limits(l, j) = lower_class_limit;
            variance_combinations(l, j) = variance + variance_combinations(i4, j - 1);
          }
        }
      }
    }
    lower_class_limits(l, 1) = 1;
    variance_combinations(l, 1) = variance;
  }

  // Extracting the class breaks
  Rcpp::NumericVector kclass(n_classes - 1);
  int k = data_length - 1;

  for (int countNum = n_classes - 1; countNum > 0; countNum--) {
    if (countNum > 0) {
      kclass[countNum - 1] = inp_data[lower_class_limits(k, countNum + 1) - 2];
    }
    k = lower_class_limits(k, countNum + 1) - 1;
  }

  return kclass;
}
