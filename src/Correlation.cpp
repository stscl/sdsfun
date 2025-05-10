#include <iostream>
#include <stdexcept>
#include <vector>
#include <cmath>
#include <numeric> // for std::accumulate
#include <limits>  // for std::numeric_limits
// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Function to compute Pearson correlation using Armadillo
double PearsonCor(const std::vector<double>& y,
                  const std::vector<double>& y_hat,
                  bool NA_rm = false) {
  // Check input sizes
  if (y.size() != y_hat.size()) {
    throw std::invalid_argument("Input vectors must have the same size.");
  }

  // Handle NA values
  std::vector<double> clean_y, clean_y_hat;
  for (size_t i = 0; i < y.size(); ++i) {
    bool is_na = std::isnan(y[i]) || std::isnan(y_hat[i]);
    if (is_na) {
      if (!NA_rm) {
        return std::numeric_limits<double>::quiet_NaN(); // Return NaN if NA_rm is false
      }
    } else {
      clean_y.push_back(y[i]);
      clean_y_hat.push_back(y_hat[i]);
    }
  }

  // If no valid data, return NaN
  if (clean_y.empty()) {
    return std::numeric_limits<double>::quiet_NaN();
  }

  // Convert cleaned vectors to Armadillo vectors
  arma::vec arma_y(clean_y);
  arma::vec arma_y_hat(clean_y_hat);

  // Compute Pearson correlation using Armadillo
  double corr = arma::as_scalar(arma::cor(arma_y, arma_y_hat));

  // Ensure correlation is within valid range [-1, 1]
  if (corr < -1.0) corr = -1.0;
  if (corr > 1.0) corr = 1.0;

  return corr;
}

/*
 * Function to compute Partial Correlation using Armadillo
 *
 * Computes the partial correlation between the dependent variable 'y' and the predicted variable 'y_hat',
 * after controlling for the variables specified in the 'controls' matrix. The partial correlation can be computed
 * either through linear regression or by using the correlation matrix, depending on the 'linear' flag.
 * Optionally, missing values (NA) can be removed if 'NA_rm' is set to true.
 *
 * Parameters:
 *   y          - A vector representing the dependent variable.
 *   y_hat      - A vector representing the predicted variable.
 *   controls   - A matrix where each row corresponds to a control variable to adjust for in the correlation.
 *   NA_rm      - A boolean flag to indicate whether to remove missing values (default is false).
 *   linear     - A boolean flag to specify whether to use linear regression (true) or correlation matrix (false)
 *                for computing the partial correlation (default is false).
 *
 * Returns:
 *   A double representing the partial correlation coefficient between 'y' and 'y_hat' after controlling for
 *   the variables in 'controls'.
 */
double PartialCor(const std::vector<double>& y,
                  const std::vector<double>& y_hat,
                  const std::vector<std::vector<double>>& controls,
                  bool NA_rm = false,
                  bool linear = false) {
  // Check input sizes
  if (y.size() != y_hat.size()) {
    throw std::invalid_argument("Input vectors y and y_hat must have the same size.");
  }
  if (!controls.empty()) {
    bool all_controls_valid = std::all_of(controls.begin(), controls.end(),
                                          [&](const std::vector<double>& ctrl) { return ctrl.size() == y.size(); });
    if (!all_controls_valid) {
      throw std::invalid_argument("All control variables must have the same size as y.");
    }
  }

  // Handle NA values
  std::vector<double> clean_y, clean_y_hat;
  std::vector<std::vector<double>> clean_controls(controls.size());

  for (size_t i = 0; i < y.size(); ++i) {
    bool is_na = std::isnan(y[i]) || std::isnan(y_hat[i]);
    for (const auto& control : controls) {
      if (std::isnan(control[i])) {
        is_na = true;
        break;
      }
    }
    if (is_na) {
      if (!NA_rm) {
        return std::numeric_limits<double>::quiet_NaN(); // Return NaN if NA_rm is false
      }
    } else {
      clean_y.push_back(y[i]);
      clean_y_hat.push_back(y_hat[i]);
      for (size_t j = 0; j < controls.size(); ++j) {
        clean_controls[j].push_back(controls[j][i]);
      }
    }
  }

  // If no valid data, return NaN
  if (clean_y.empty()) {
    return std::numeric_limits<double>::quiet_NaN();
  }

  // Check sample adequacy
  if (clean_y.size() <= (clean_controls.size() + 2)) {  // n_samples > n_vars
    return std::numeric_limits<double>::quiet_NaN();
  }

  double partial_corr;
  if (linear){
    // Convert cleaned vectors to Armadillo vectors/matrices
    arma::vec arma_y(clean_y);
    arma::vec arma_y_hat(clean_y_hat);
    arma::mat arma_controls(clean_y.size(), controls.size());
    for (size_t i = 0; i < controls.size(); ++i) {
      arma_controls.col(i) = arma::vec(clean_controls[i]);
    }

    // // Compute residuals of y and y_hat after regressing on controls
    // arma::vec residuals_y = arma_y - arma_controls * arma::solve(arma_controls, arma_y);
    // arma::vec residuals_y_hat = arma_y_hat - arma_controls * arma::solve(arma_controls, arma_y_hat);

    // Use a more robust method for solving the linear system, such as arma::pinv (pseudo-inverse):
    arma::vec residuals_y = arma_y - arma_controls * arma::pinv(arma_controls) * arma_y;
    arma::vec residuals_y_hat = arma_y_hat - arma_controls * arma::pinv(arma_controls) * arma_y_hat;

    // Compute Pearson correlation of the residuals
    partial_corr = arma::as_scalar(arma::cor(residuals_y, residuals_y_hat));

  } else {
    int i = controls.size();
    int j = controls.size() + 1;
    arma::mat data(clean_y.size(), i + 2);
    for (size_t k = 0; k < controls.size(); ++k) {
      data.col(k) = arma::vec(clean_controls[k]);
    }
    data.col(i) = arma::vec(clean_y);
    data.col(j) = arma::vec(clean_y_hat);

    if (data.n_rows < 2 || data.n_cols < 1) {
      return std::numeric_limits<double>::quiet_NaN();
    }

    // Compute the correlation matrix of the data
    arma::mat corrm = arma::cor(data);

    // // Compute the precision matrix (inverse of the correlation matrix)
    // arma::mat precm = arma::inv(corrm);

    // Moore-Penrose pseudo-inverse
    // arma::mat precm = arma::pinv(corrm);
    arma::mat precm;
    try {
      precm = arma::pinv(corrm, 1e-10);
    } catch (...) {
      return std::numeric_limits<double>::quiet_NaN();
    }

    // Get the correlation between y and y_hat after controlling for the others
    partial_corr = -precm(i, j) / std::sqrt(precm(i, i) * precm(j, j));
  }

  // Ensure partial correlation is within valid range [-1, 1]
  if (partial_corr < -1.0) partial_corr = -1.0;
  if (partial_corr > 1.0) partial_corr = 1.0;

  return partial_corr;
}

double PartialCorTrivar(const std::vector<double>& y,
                        const std::vector<double>& y_hat,
                        const std::vector<double>& control,
                        bool NA_rm = false,
                        bool linear = false){
  std::vector<std::vector<double>> conmat;
  conmat.push_back(control);

  double res = PartialCor(y,y_hat,conmat,NA_rm,linear);
  return res;
}

/**
 * Calculates the significance (two-sided p-value) of a (partial) correlation coefficient.
 *
 * This function computes the t-statistic for a given (partial) correlation coefficient `r`
 * and returns the corresponding two-tailed p-value under the null hypothesis that the true
 * correlation is zero.
 *
 * The t-statistic is calculated using:
 *     t = r * sqrt((n - k - 2) / (1 - r^2))
 * where:
 *     - r is the correlation coefficient
 *     - n is the sample size
 *     - k is the number of control variables (0 for simple correlation)
 *
 * The degrees of freedom used is (n - k - 2). The resulting two-sided p-value is computed
 * using the cumulative distribution function of the t-distribution.
 *
 * @param r The (partial) correlation coefficient.
 * @param n The number of observations.
 * @param k The number of control variables (default = 0).
 * @return The two-sided p-value.
 */
double CppCorSignificance(double r, int n, int k = 0) {
  double df = n - k - 2;
  double t = r * std::sqrt(df / (1 - r * r));

  double pvalue = (1 - R::pt(t, df, true, false)) * 2;
  // Ensure p value is within valid range [-1, 1]
  if (pvalue < 0) pvalue = 0;
  if (pvalue > 1.0) pvalue = 1.0;

  return pvalue;
}

/**
 * Calculates the confidence interval for a (partial) correlation coefficient.
 *
 * This function uses Fisher's z-transformation to compute the confidence interval
 * for a correlation or partial correlation coefficient `r`. The transformation
 * stabilizes the variance of `r` for more accurate interval estimation.
 *
 * The steps include:
 *   1. Transforming r using Fisher's z.
 *   2. Computing the standard error of z.
 *   3. Determining the critical z-value for the specified confidence level.
 *   4. Calculating the confidence interval in the z-domain.
 *   5. Back-transforming to get the interval in the correlation domain.
 *
 * The degrees of freedom are adjusted for partial correlation with `k` control variables.
 *
 * @param r The (partial) correlation coefficient.
 * @param n The number of observations.
 * @param k The number of control variables (default = 0; use 0 for simple correlation).
 * @param level The significance level α for the confidence interval (default = 0.05).
 * @return A vector containing the upper and lower bounds of the confidence interval.
 */
std::vector<double> CppCorConfidence(double r, int n, int k = 0,
                                     double level = 0.05) {
  // Calculate the Fisher's z-transformation
  double z = 0.5 * std::log((1 + r) / (1 - r));

  // Calculate the standard error of z
  double ztheta = 1 / std::sqrt(n - k - 3);

  // Calculate the z-value for the given confidence level
  double qZ = R::qnorm(1 - level / 2, 0.0, 1.0, true, false);

  // Calculate the upper and lower bounds of the confidence interval
  double upper = z + qZ * ztheta;
  double lower = z - qZ * ztheta;

  // Convert the bounds back to correlation coefficients
  double r_upper = (std::exp(2 * upper) - 1) / (std::exp(2 * upper) + 1);
  double r_lower = (std::exp(2 * lower) - 1) / (std::exp(2 * lower) + 1);

  // Return the result as a std::vector<double>
  return {r_upper, r_lower};
}
