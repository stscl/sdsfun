#include <RcppArmadillo.h>
#include <limits>  // for std::numeric_limits

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

arma::mat calculate_variances(const arma::vec& data, int nclass) {
  int n = data.n_elem;
  arma::mat variance_matrix(n, nclass, arma::fill::zeros);
  arma::mat break_matrix(n, nclass, arma::fill::zeros);

  // Initialize variance_matrix with positive infinity
  variance_matrix.fill(std::numeric_limits<double>::infinity());

  arma::vec sum1(n, arma::fill::zeros);  // Prefix sum
  arma::vec sum2(n, arma::fill::zeros);  // Prefix sum of squares

  // Compute prefix sums
  for (int i = 0; i < n; ++i) {
    sum1[i] = (i == 0 ? 0 : sum1[i-1]) + data[i];
    sum2[i] = (i == 0 ? 0 : sum2[i-1]) + data[i] * data[i];
  }

  // Calculate variance for 1-class solutions
  for (int i = 1; i < n; ++i) {
    for (int j = i; j < n; ++j) {
      double s1 = sum1[j] - (i > 0 ? sum1[i - 1] : 0);
      double s2 = sum2[j] - (i > 0 ? sum2[i - 1] : 0);
      double mean = s1 / (j - i + 1);
      double variance = s2 - s1 * mean;

      variance_matrix(j, 0) = std::min(variance_matrix(j, 0), variance);
    }
  }

  // Calculate variance for multi-class solutions
  for (int k = 1; k < nclass; ++k) {
    for (int j = 1; j < n; ++j) {
      double min_variance = std::numeric_limits<double>::infinity();
      int best_index = -1;

      // Try all possible splits
      for (int i = 0; i < j; ++i) {
        double variance = variance_matrix(i, k - 1) + (sum2[j] - sum2[i]);
        if (variance < min_variance) {
          min_variance = variance;
          best_index = i;
        }
      }

      variance_matrix(j, k) = min_variance;
      break_matrix(j, k) = best_index;
    }
  }

  return break_matrix;
}

arma::vec GetJenksBreaks(const arma::vec& x, int nclass) {
  arma::vec sorted_x = arma::sort(x);  // Sort the data
  int n = sorted_x.n_elem;

  // Get the breakpoints matrix
  arma::mat breaks_matrix = calculate_variances(sorted_x, nclass);

  // Determine breakpoints from the break matrix
  arma::vec breaks(nclass + 1);
  breaks[nclass] = sorted_x[n - 1];

  for (int k = nclass - 1; k > 0; --k) {
    breaks[k] = sorted_x[breaks_matrix(n - 1, k)];
    n = breaks_matrix(n - 1, k);
  }
  breaks[0] = sorted_x[0];

  return breaks;
}

// [[Rcpp::export]]
Rcpp::IntegerVector naturalDisc(const arma::vec& x, int n) {
  arma::vec breaks = GetJenksBreaks(x, n);

  Rcpp::IntegerVector result(x.n_elem);

  // Assign each data point to a class based on breakpoints
  for (size_t i = 0; i < x.n_elem; ++i) {
    for (int j = 0; j < n; ++j) {
      if (x[i] <= breaks[j + 1]) {
        result[i] = j + 1;
        break;
      }
    }
  }

  return result;
}
