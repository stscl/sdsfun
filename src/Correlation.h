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
 *
 * Returns:
 *   A double representing the partial correlation coefficient between 'y' and 'y_hat' after controlling for
 *   the variables in 'controls'.
 */
double PartialCor(const std::vector<double>& y,
                  const std::vector<double>& y_hat,
                  const std::vector<std::vector<double>>& controls,
                  bool NA_rm = false,
                  bool linear = false);

double PartialCorTrivar(const std::vector<double>& y,
                        const std::vector<double>& y_hat,
                        const std::vector<double>& control,
                        bool NA_rm = false,
                        bool linear = false);

// Function to calculate the significance of a (partial) correlation coefficient
// r: (partial) correlation coefficient
// n: number of observations
// k: number of control variables (0 for simple correlation, >0 for partial)
// Returns the two-sided p-value
double CppCorSignificance(double r, int n, int k = 0);

// Function to calculate the confidence interval for a (partial) correlation coefficient
// r: (partial) correlation coefficient
// n: number of observations
// k: number of control variables (0 for simple correlation, >0 for partial)
// level: the significance level (α) for the confidence interval.
// Returns the upper and lower bound of the confidence interval for r
std::vector<double> CppCorConfidence(double r, int n, int k = 0,
                                     double level = 0.05);

#endif // Correlation_H
