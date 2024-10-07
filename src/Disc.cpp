#include <RcppArmadillo.h>
#include <limits>  // for std::numeric_limits
#include <random>  // For sampling

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

// Helper function to calculate variance matrix
arma::mat calculate_variances(const arma::vec& sorted_x, int n) {
  int nx = sorted_x.n_elem;
  arma::mat variance(nx, n, arma::fill::zeros);
  arma::mat indexes(nx, n, arma::fill::zeros);

  // Initialize arrays for means and squared sums
  arma::vec sums(nx, arma::fill::zeros);
  arma::vec sumsq(nx, arma::fill::zeros);

  // First class
  for (int i = 0; i < nx; ++i) {
    sums[i] = sorted_x[i] + (i > 0 ? sums[i - 1] : 0);
    sumsq[i] = sorted_x[i] * sorted_x[i] + (i > 0 ? sumsq[i - 1] : 0);
    double variance_val = (i > 0) ? sumsq[i] - (sums[i] * sums[i] / (i + 1)) : 0;
    variance(i, 0) = variance_val;
    indexes(i, 0) = i;
  }

  // Subsequent classes
  for (int j = 1; j < n; ++j) {
    for (int i = j; i < nx; ++i) {
      double min_var = std::numeric_limits<double>::infinity();
      int best_index = 0;

      for (int k = 0; k < i; ++k) {
        double mean_ik = sums[i] - sums[k];
        double sumsq_ik = sumsq[i] - sumsq[k];
        double variance_ik = sumsq_ik - (mean_ik * mean_ik / (i - k));

        double total_variance = variance(k, j - 1) + variance_ik;

        if (total_variance < min_var) {
          min_var = total_variance;
          best_index = k;
        }
      }

      variance(i, j) = min_var;
      indexes(i, j) = best_index;
    }
  }

  return indexes;
}

// Main function to calculate Jenks breaks
// [[Rcpp::export]]
arma::vec GetJenksBreaks(const arma::vec& x, int n) {
  arma::vec sorted_x = arma::sort(x);  // Sort the data
  int nx = sorted_x.n_elem;

  // If n <= 2, simply return min and max of x
  if (n <= 2) {
    arma::vec simple_breaks(2);
    simple_breaks[0] = sorted_x[0];         // Minimum value of x
    simple_breaks[1] = sorted_x[nx - 1];    // Maximum value of x
    return simple_breaks;
  }

  // Compute variance matrix
  arma::mat breaks_matrix = calculate_variances(sorted_x, n);

  // Initialize breakpoints vector
  arma::vec breaks(n + 1);
  breaks[n] = sorted_x[nx - 1];  // Set the maximum value

  // Backtrack to find breakpoints
  for (int k = n - 1; k > 0; --k) {
    breaks[k] = sorted_x[breaks_matrix(nx - 1, k)];
    nx = breaks_matrix(nx - 1, k);
  }

  // Set the minimum value
  breaks[0] = sorted_x[0];

  return breaks;
}

// [[Rcpp::export]]
Rcpp::IntegerVector naturalDisc(const arma::vec& x,
                                int n, double sampleprob) {
  arma::vec data = x;  // Copy of input data
  arma::vec breaks;

  // Check if sampling is needed
  if (x.n_elem > 3000) {
    // Calculate sample size based on sample probability
    int sample_size = static_cast<int>(std::round(x.n_elem * sampleprob));

    // Ensure sample size is within valid range
    if (sample_size < 1) {
      Rcpp::stop("Sample size is too small");
    }

    // Generate random indices for sampling
    arma::uvec indices = arma::randperm(x.n_elem, sample_size);

    // Sample the data
    data = x.elem(indices);
  }

  // Compute Jenks breaks using sampled data (or full data if no sampling)
  breaks = GetJenksBreaks(data, n);

  Rcpp::IntegerVector result(x.n_elem);

  // Assign each data point to a class based on the computed breakpoints
  for (size_t i = 0; i < x.n_elem; ++i) {
    for (int j = 0; j < n; ++j) {
      if (x[i] < breaks[j + 1]) {
        result[i] = j + 1;
        break;
      }
    }
    if (x[i] == breaks[breaks.n_elem - 1]) {
      result[i] = breaks.n_elem - 1;
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
