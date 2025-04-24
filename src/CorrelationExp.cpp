#include <vector>
#include "Correlation.h"
// 'Rcpp.h' should not be included and correct to include only 'RcppArmadillo.h'.
// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericVector RcppPearsonCor(const Rcpp::NumericVector& vec1,
                                   const Rcpp::NumericVector& vec2,
                                   double level = 0.05) {
  // Convert Rcpp::NumericVector to std::vector<double>
  std::vector<double> v1 = Rcpp::as<std::vector<double>>(vec1);
  std::vector<double> v2 = Rcpp::as<std::vector<double>>(vec2);

  int n = static_cast<int>(v1.size());

  // The Pearson correlation coefficient
  double r = PearsonCor(v1, v2, true);

  // The P value of the Pearson correlation coefficient
  double p = CppCorSignificance(r, n, 0);

  // Confidence interval of the correlation coefficient
  std::vector<double> conf = CppCorConfidence(r, n, 0, level);

  // Convert the result back to Rcpp::NumericVector
  std::vector<double> res_std = {r, p, conf[1], conf[0]};
  Rcpp::NumericVector res = Rcpp::wrap(res_std);
  res.names() = Rcpp::CharacterVector::create(
    "correlation",
    "significance",
    "upper confidence interval",
    "lower confidence interval"
  );

  return res;
}
