#include <RcppArmadillo.h>
#include <limits>
#include <random>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::IntegerVector sdDisc(const arma::vec& x, double n) {
  double mean_x = arma::mean(x);
  double std_x = arma::stddev(x);

  Rcpp::IntegerVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    double diff = (x[i] - mean_x) / std_x;
    int index = std::floor((diff + n/2) / n * n);
    result[i] = std::min(std::max(1, index), (int)n);
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::IntegerVector equalDisc(const arma::vec& x, double n) {
  double min_x = arma::min(x);
  double max_x = arma::max(x);
  double interval = (max_x - min_x) / n;

  Rcpp::IntegerVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    int index = std::ceil((x[i] - min_x) / interval);
    result[i] = std::min(std::max(1, index), (int)n);
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::IntegerVector geometricDisc(const arma::vec& x, double n) {
  double min_x = arma::min(x);
  double max_x = arma::max(x);
  double factor = std::pow(max_x / min_x, 1.0 / n);

  Rcpp::IntegerVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    int index = std::floor(std::log(x[i] / min_x) / std::log(factor)) + 1;
    result[i] = std::min(std::max(1, index), (int)n);
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::IntegerVector quantileDisc(const arma::vec& x, double n) {
  arma::vec sorted_x = arma::sort(x);
  arma::vec quantiles(n + 1);

  for (int i = 0; i <= n; ++i) {
    quantiles[i] = sorted_x[i * (sorted_x.n_elem - 1) / n];
  }

  Rcpp::IntegerVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    for (int j = 0; j < n; ++j) {
      if (x[i] <= quantiles[j + 1]) {
        result[i] = j + 1;
        break;
      }
    }
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::IntegerVector manualDisc(const arma::vec& x, arma::vec breakpoint) {
  // Check if the minimum value of x is in breakpoint
  double min_x = x.min();
  if (arma::any(breakpoint > min_x) == false) {
    breakpoint.insert_rows(0, arma::vec({min_x}));
  }

  // Check if the maximum value of x is in breakpoint
  double max_x = x.max();
  if (arma::any(breakpoint < max_x) == false) {
    breakpoint.insert_rows(breakpoint.n_elem, arma::vec({max_x}));
  }

  // Sort breakpoint in ascending order
  breakpoint = arma::sort(breakpoint);

  // Initialize result vector
  Rcpp::IntegerVector result(x.n_elem);

  // Classify x based on breakpoint (left-closed, right-open intervals)
  for (size_t i = 0; i < x.n_elem; ++i) {
    for (size_t j = 0; j < breakpoint.n_elem - 1; ++j) {
      if (x[i] < breakpoint[j + 1]) {
        result[i] = j + 1;
        break;
      }
    }
    // If the value is equal to the last breakpoint, classify it in the last category
    if (x[i] == breakpoint[breakpoint.n_elem - 1]) {
      result[i] = breakpoint.n_elem - 1;
    }
  }

  return result;
}
