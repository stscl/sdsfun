#include <cmath>
#include "SDSUtils.h"
// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double CalcFactorQ(Rcpp::NumericVector y, Rcpp::IntegerVector h) {
  int N = y.size();

  // Global mean of y
  double y_mean = Rcpp::mean(y);

  // Sum of squared differences for the denominator (global variance)
  double denom = 0;
  for (int i = 0; i < N; ++i) {
    denom += std::pow(y[i] - y_mean, 2);
  }

  // Get unique levels of h
  Rcpp::IntegerVector unique_levels = RcppUnique(h);

  // Sum of squared differences within each level of h for the numerator
  double numer = 0;
  for (int level : unique_levels) {
    // Get indices corresponding to the current level
    Rcpp::NumericVector y_h;
    for (int i = 0; i < N; ++i) {
      if (h[i] == level) {
        y_h.push_back(y[i]);
      }
    }

    // Mean of y for the current level
    double y_h_mean = Rcpp::mean(y_h);

    // Sum of squared differences for the current level
    for (int i = 0; i < y_h.size(); ++i) {
      numer += std::pow(y_h[i] - y_h_mean, 2);
    }
  }

  // Calculate q value
  double q = 1 - (numer / denom);

  return q;
}

// [[Rcpp::export]]
Rcpp::List GDFactorQ(Rcpp::NumericVector y, Rcpp::IntegerVector h) {
  int N = y.size();

  // Calculate Q value
  double qv = CalcFactorQ(y, h);

  // Get unique levels of h
  Rcpp::IntegerVector unique_levels = RcppUnique(h);
  int L = unique_levels.size();

  // Calculate Fv (test statistic)
  double Fv = ((N - L) * qv) / ((L - 1) * (1 - qv));

  // Compute means and sizes for each level of h
  Rcpp::NumericVector hmean(L);
  Rcpp::NumericVector Nh(L);

  for (int j = 0; j < L; ++j) {
    int level = unique_levels[j];
    Rcpp::NumericVector y_h;

    for (int i = 0; i < N; ++i) {
      if (h[i] == level) {
        y_h.push_back(y[i]);
      }
    }

    hmean[j] = Rcpp::mean(y_h);
    Nh[j] = y_h.size();
  }

  // Calculate v1 and v2
  double v1 = Rcpp::sum(Rcpp::pow(hmean, 2.0));
  double v2 = std::pow(Rcpp::sum(Rcpp::sqrt(Nh) * hmean), 2.0) / N;

  // double v2 = Rcpp::pow(Rcpp::sum(Rcpp::sqrt(Nh) * hmean), 2.0) / N;

  // Calculate variance and lambda
  double var_y = var(y) * (N - 1) / N;
  double lambda = (v1 - v2) / var_y;

  // Obtaining namespace of stats package
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("stats");
  // Picking up pf() function from Matrix package
  Rcpp::Function RStatsPf = pkg["pf"];
  // Use R's stats::pf function to compute p-value
  double pv = Rcpp::as<double>(RStatsPf(Fv, L - 1, N - L,
                                        Rcpp::Named("ncp") = lambda,
                                        Rcpp::Named("lower.tail") = false));

  // Return both Fv and p-value
  return Rcpp::List::create(Rcpp::Named("Qvalue") = qv,
                            Rcpp::Named("Pvalue") = pv);
}
