#include <Rcpp.h>
using namespace Rcpp;

// Function to return unique elements while preserving the original order
// [[Rcpp::export]]
IntegerVector Runique(IntegerVector x) {
  std::vector<int> seen;
  std::vector<int> result;

  for (int i = 0; i < x.size(); i++) {
    // If the value has not been seen before, add it to the result
    if (std::find(seen.begin(), seen.end(), x[i]) == seen.end()) {
      seen.push_back(x[i]);
      result.push_back(x[i]);
    }
  }

  return wrap(result);
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix DummyVar(Rcpp::IntegerVector x) {
  // Get the number of unique levels
  Rcpp::IntegerVector levels = Runique(x);
  int n = x.size();
  int k = levels.size();

  // Create an n-row by k-column matrix to store the dummy variables
  Rcpp::IntegerMatrix dummy(n, k);

  // Iterate over the input vector x
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < k; ++j) {
      // Set the corresponding column to 1 if the level matches, else 0
      if (x[i] == levels[j]) {
        dummy(i, j) = 1;
      } else {
        dummy(i, j) = 0;
      }
    }
  }

  return dummy;
}
