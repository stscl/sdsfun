#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericVector LinearRegression(const Rcpp::NumericVector& y, const Rcpp::NumericMatrix& X) {
  // Convert Rcpp inputs to Armadillo objects
  arma::vec y_arma = Rcpp::as<arma::vec>(y);
  arma::mat X_arma = Rcpp::as<arma::mat>(X);

  // Add a column of ones to X for the intercept
  arma::mat X_with_intercept = arma::join_rows(arma::ones(X_arma.n_rows), X_arma);

  // Solve the linear regression problem using Armadillo's least squares solver
  arma::vec coefficients = arma::solve(X_with_intercept, y_arma);

  // Return the coefficients as an Rcpp NumericVector
  return Rcpp::wrap(coefficients);
}

// [[Rcpp::export]]
Rcpp::NumericVector LinearTrendRM(const Rcpp::NumericVector& y, const Rcpp::NumericMatrix& X) {
  // Convert Rcpp inputs to Armadillo objects
  arma::vec y_arma = Rcpp::as<arma::vec>(y);
  arma::mat X_arma = Rcpp::as<arma::mat>(X);

  // Add a column of ones to X for the intercept
  arma::mat X_with_intercept = arma::join_rows(arma::ones(X_arma.n_rows), X_arma);

  // Solve the linear regression problem using Armadillo's least squares solver
  arma::vec coefficients = arma::solve(X_with_intercept, y_arma);

  // Compute the predicted values (y_hat)
  arma::vec y_hat = X_with_intercept * coefficients;

  // Compute the residuals (y - y_hat)
  arma::vec residuals = y_arma - y_hat;

  // Return the residuals as an Rcpp NumericVector
  return Rcpp::wrap(residuals);
}
