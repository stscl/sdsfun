#include <Rcpp.h>
using namespace Rcpp;

// Function to return unique elements while preserving the original order
// [[Rcpp::export]]
Rcpp::IntegerVector Runique(Rcpp::IntegerVector x) {
  std::vector<int> seen;
  std::vector<int> result;

  for (int i = 0; i < x.size(); ++i) {
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

// [[Rcpp::export]]
Rcpp::IntegerMatrix DummyMat(Rcpp::IntegerMatrix mat) {
  int n = mat.nrow();   // Number of rows
  int p = mat.ncol();   // Number of columns

  // Calculate the total number of dummy variables needed
  int total_dummy_cols = 0;

  // Store the number of dummy columns generated for each column in the matrix
  std::vector<int> dummy_col_count(p);

  // Loop to count the total dummy columns
  for (int col_idx = 0; col_idx < p; ++col_idx) {
    Rcpp::IntegerVector x = mat(_, col_idx);
    Rcpp::IntegerVector levels = Runique(x);
    dummy_col_count[col_idx] = levels.size();
    total_dummy_cols += levels.size();
  }

  // Create an IntegerMatrix to store the dummy variables
  Rcpp::IntegerMatrix dummy_matrix(n, total_dummy_cols);
  int current_col = 0;

  // Loop through each column of the input matrix to create dummy variables
  for (int col_idx = 0; col_idx < p; ++col_idx) {
    Rcpp::IntegerVector x = mat(_, col_idx);
    Rcpp::IntegerVector levels = Runique(x);  // Get unique levels

    // Create dummy variables for each level
    for (int level_idx = 0; level_idx < levels.size(); ++level_idx) {
      int current_level = levels[level_idx];

      // Fill in the dummy variable column
      for (int i = 0; i < n; i++) {
        dummy_matrix(i, current_col) = (x[i] == current_level) ? 1 : 0;
      }

      // Move to the next column in the dummy matrix
      current_col++;
    }
  }

  return dummy_matrix;  // Return the resulting dummy matrix
}
