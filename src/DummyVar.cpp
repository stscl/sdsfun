#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix DummyVar(IntegerVector x) {
  // Get the number of unique levels
  IntegerVector levels = sort_unique(x);
  int n = x.size();
  int k = levels.size();

  // Create an n-row by k-column matrix to store the dummy variables
  NumericMatrix dummy(n, k);

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
