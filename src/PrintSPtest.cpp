#include <iomanip>
#include <sstream>
#include <string>
#include "SDSUtils.h"
// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Helper function to concatenate a number and a significance star
std::string ConcatNumStar(double num, double p) {
  std::ostringstream oss;
  oss << num << SignificanceStar(p);
  return oss.str();
}

// Function to print the global spatial autocorrelation test results
// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame PrintGlobalMoranI(Rcpp::DataFrame df) {
  Rcpp::CharacterVector variable = df["Variable"];
  Rcpp::NumericVector moran_i = df["MoranI"];
  Rcpp::NumericVector ei = df["EI"];
  Rcpp::NumericVector vari = df["VarI"];
  Rcpp::NumericVector zi = df["ZI"];
  Rcpp::NumericVector pi = df["PI"];
  Rcpp::CharacterVector stars(variable.size());

  for (int i = 0; i < variable.size(); ++i) {
    stars[i] = ConcatNumStar(moran_i[i],pi[i]);
  }

  Rcpp::DataFrame out = Rcpp::DataFrame::create(Rcpp::Named("Variable") = variable,
                                                Rcpp::Named("MoranI") = stars,
                                                Rcpp::Named("EI") = ei,
                                                Rcpp::Named("VarI") = vari,
                                                Rcpp::Named("zI") = zi,
                                                Rcpp::Named("pI") = pi);

  return out;
}
