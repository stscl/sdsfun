#include <limits>
#include <random>
#include <algorithm>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export(rng = false)]]
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

// [[Rcpp::export(rng = false)]]
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

// [[Rcpp::export(rng = false)]]
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

// [[Rcpp::export(rng = false)]]
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

// [[Rcpp::export(rng = false)]]
Rcpp::IntegerVector manualDisc(const arma::vec& x, arma::vec breakpoint) {
  double min_x = x.min();
  double max_x = x.max();

  // Check if the minimum value of x is in breakpoint
  if (arma::all(breakpoint > min_x) == true) {
    breakpoint.insert_rows(0, arma::vec({min_x}));
  }

  // Check if the maximum value of x is in breakpoint
  if (arma::all(breakpoint < max_x) == true) {
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

// [[Rcpp::export(rng = false)]]
arma::vec ArmaJenksBreaks(const arma::vec& inp_data, int n_classes,
                          bool is_sorted = false) {

  arma::vec data = inp_data;
  if (!is_sorted) {
    data = arma::sort(data);
  }

  int data_length = data.n_elem;

  // Initialize matrices for lower class limits and variance combinations
  arma::imat lower_class_limits(data_length + 1, n_classes + 1, arma::fill::zeros);
  arma::mat variance_combinations(data_length + 1, n_classes + 1);
  variance_combinations.fill(arma::datum::inf);

  for (int i = 1; i <= n_classes; ++i) {
    lower_class_limits(1, i) = 1;
    variance_combinations(1, i) = 0.0;
  }

  for (int l = 2; l <= data_length; ++l) {
    double sum = 0.0, sum_squares = 0.0, variance = 0.0;

    for (int m = 1; m <= l; ++m) {
      int lower_class_limit = l - m + 1;
      double val = data[lower_class_limit - 1];
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
    variance_combinations(l, 1) = variance;  // 'variance' now properly declared and used
  }

  // Extracting the class breaks
  arma::vec kclass(n_classes - 1, arma::fill::zeros);
  int k = data_length - 1;

  for (int countNum = n_classes - 1; countNum > 0; countNum--) {
    if (countNum > 0) {
      kclass[countNum - 1] = data[lower_class_limits(k, countNum + 1) - 2];
    }
    k = lower_class_limits(k, countNum + 1) - 1;
  }

  return kclass;
}

// [[Rcpp::export(rng = false)]]
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
  breaks = ArmaJenksBreaks(data, n, false);
  int bn = breaks.n_elem;

  Rcpp::IntegerVector result(x.n_elem);

  // Assign each data point to a class based on the computed breakpoints
  for (size_t i = 0; i < x.n_elem; ++i) {
    for (int j = 0; j < bn; ++j) {
      if (x[i] < breaks[j]) {
        result[i] = j + 1;
        break;
      }
    }
    if (x[i] >= breaks[breaks.n_elem - 1]) {
      result[i] = n;
    }
  }

  return result;
}

// [[Rcpp::export(rng = false)]]
Rcpp::IntegerVector htDisc(const arma::vec& x, double thr = 0.4) {
  // Remove NA and non-finite values for break calculation
  arma::vec x_clean = x.elem(arma::find_finite(x));
  arma::vec head = x_clean;

  std::vector<double> breaks;
  breaks.push_back(x_clean.min());

  for (int i = 0; i < 100; ++i) {
    double mu = arma::mean(head);
    breaks.push_back(mu);

    int ntot = head.n_elem;
    head = head.elem(arma::find(head > mu));
    double prop = static_cast<double>(head.n_elem) / ntot;

    if (prop > thr || head.n_elem <= 1) {
      break;
    }
  }

  // Add maximum value to complete the break interval
  breaks.push_back(x_clean.max());

  // Remove duplicates and sort
  std::sort(breaks.begin(), breaks.end());
  breaks.erase(std::unique(breaks.begin(), breaks.end()), breaks.end());

  arma::vec brks = arma::conv_to<arma::vec>::from(breaks);
  int n_breaks = brks.n_elem;

  // Output vector
  Rcpp::IntegerVector result(x.n_elem);

  for (size_t i = 0; i < x.n_elem; ++i) {
    double val = x[i];
    if (!std::isfinite(val)) {
      result[i] = NA_INTEGER;
    } else {
      int label = 1;
      for (int j = 0; j < n_breaks - 1; ++j) {
        if (val < brks[j + 1]) {
          label = j + 1;
          break;
        }
      }
      if (val >= brks[n_breaks - 1]) {
        label = n_breaks - 1;
      }
      result[i] = label;
    }
  }

  return result;
}
