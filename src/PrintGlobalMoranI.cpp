#include <Rcpp.h>
#include <iomanip>
#include <sstream>
#include <string>

using namespace Rcpp;

// Function to calculate the star based on p-value
std::string star(double p) {
  if (p < 0.001) return "***";
  if (p < 0.01) return "**";
  if (p < 0.05) return "*";
  if (p < 0.1) return ".";
  return " ";
}

// Helper function to concatenate a number and a significance star
std::string concat_num_with_star(double num, double p) {
  std::ostringstream oss;
  oss << num << star(p);
  return oss.str();
}

// Function to print the global spatial autocorrelation test results
// [[Rcpp::export]]
Rcpp::DataFrame PrintGlobalMoranI(Rcpp::DataFrame df) {
  Rcpp::CharacterVector variable = df["Variable"];
  Rcpp::NumericVector moran_i = df["MoranI"];
  Rcpp::NumericVector ei = df["EI"];
  Rcpp::NumericVector vari = df["VarI"];
  Rcpp::NumericVector zi = df["ZI"];
  Rcpp::NumericVector pi = df["PI"];
  Rcpp::CharacterVector stars(variable.size());

  for (int i = 0; i < variable.size(); ++i) {
    stars[i] = concat_num_with_star(moran_i[i],pi[i]);
  }

  Rcpp::DataFrame out = Rcpp::DataFrame::create(Rcpp::Named("Variable") = variable,
                                                Rcpp::Named("MoranI") = stars,
                                                Rcpp::Named("EI") = ei,
                                                Rcpp::Named("VarI") = vari,
                                                Rcpp::Named("zI") = zi,
                                                Rcpp::Named("pI") = pi);

  return out;
}
