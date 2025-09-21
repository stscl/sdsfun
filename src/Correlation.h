#ifndef Correlation_H
#define Correlation_H

#include <iostream>
#include <stdexcept>
#include <vector>
#include <cmath>
#include <numeric> // for std::accumulate
#include <limits>  // for std::numeric_limits
// #include <Rcpp.h>
#include <RcppArmadillo.h>

double PearsonCor(const std::vector<double>& y,
                  const std::vector<double>& y_hat,
                  bool NA_rm = false);


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
 *   pinv_tol   - Tolerance used for the pseudo-inverse (arma::pinv). Smaller values increase precision but may be less stable
 *                (default is 1e-10).
 *
 * Returns:
 *   A double representing the partial correlation coefficient between 'y' and 'y_hat' after controlling for
 *   the variables in 'controls'.
 */
double PartialCor(const std::vector<double>& y,
                  const std::vector<double>& y_hat,
                  const std::vector<std::vector<double>>& controls,
                  bool NA_rm = false,
                  bool linear = false,
                  double pinv_tol = 1e-10);

double PartialCorTrivar(const std::vector<double>& y,
                        const std::vector<double>& y_hat,
                        const std::vector<double>& control,
                        bool NA_rm = false,
                        bool linear = false);

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
double CppCorSignificance(double r, int n, int k = 0);

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
                                     double level = 0.05);

#endif // Correlation_H
