#include <Rcpp.h>
using namespace Rcpp;

// Function to return unique elements while preserving the original order
// [[Rcpp::export]]
Rcpp::IntegerVector RcppUnique(Rcpp::IntegerVector x) {
  std::vector<int> seen;
  std::vector<int> result;

  for (int i = 0; i < x.size(); ++i) {
    // If the value has not been seen before, add it to the result
    if (std::find(seen.begin(), seen.end(), x[i]) == seen.end()) {
      seen.push_back(x[i]);
      result.push_back(x[i]);
    }
  }

  return Rcpp::wrap(result);
}
