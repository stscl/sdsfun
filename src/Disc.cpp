#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericVector sdDisc(const arma::vec& x, double n) {
  double mean_x = arma::mean(x);
  double std_x = arma::stddev(x);

  Rcpp::NumericVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    double diff = x[i] - mean_x;
    result[i] = std::round(diff / std_x * n);
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector equalDisc(const arma::vec& x, double n) {
  double min_x = arma::min(x);
  double max_x = arma::max(x);
  double interval = (max_x - min_x) / n;

  Rcpp::NumericVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    result[i] = std::floor((x[i] - min_x) / interval);
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector geometricDisc(const arma::vec& x, double n) {
  double min_x = arma::min(x);
  double max_x = arma::max(x);
  double factor = std::pow(max_x / min_x, 1.0 / n);

  Rcpp::NumericVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    result[i] = std::floor(std::log(x[i] / min_x) / std::log(factor));
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector quantileDisc(const arma::vec& x, double n) {
  arma::vec sorted_x = arma::sort(x);
  arma::vec quantiles(n);

  for (int i = 1; i < n; ++i) {
    quantiles[i-1] = sorted_x[i * sorted_x.n_elem / n];
  }
  quantiles[n-1] = sorted_x.back();

  Rcpp::NumericVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    for (int j = 0; j < n; ++j) {
      if (x[i] <= quantiles[j]) {
        result[i] = j;
        break;
      }
    }
  }

  return result;
}


