#include <Rcpp.h>
#include "SDSUtils.h"
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerMatrix DummyVar(Rcpp::IntegerVector x) {
  // Get the number of unique levels
  Rcpp::IntegerVector levels = RcppUnique(x);
  int n = x.size();
  int k = levels.size();

  // We will generate k-1 dummy variables (excluding the last level as reference)
  Rcpp::IntegerMatrix dummy(n, k - 1);

  // Iterate over the input vector x
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < k - 1; ++j) {
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
    Rcpp::IntegerVector levels = RcppUnique(x);
    dummy_col_count[col_idx] = levels.size() - 1;  // n-1 dummy variables
    total_dummy_cols += levels.size() - 1;
  }

  // Create an IntegerMatrix to store the dummy variables
  Rcpp::IntegerMatrix dummy_matrix(n, total_dummy_cols);
  int current_col = 0;

  // Loop through each column of the input matrix to create dummy variables
  for (int col_idx = 0; col_idx < p; ++col_idx) {
    Rcpp::IntegerVector x = mat(_, col_idx);
    Rcpp::IntegerVector levels = RcppUnique(x);  // Get unique levels

    // Create dummy variables for each level, except the last one
    for (int level_idx = 0; level_idx < levels.size() - 1; ++level_idx) {
      int current_level = levels[level_idx];

      // Fill in the dummy variable column
      for (int i = 0; i < n; ++i) {
        dummy_matrix(i, current_col) = (x[i] == current_level) ? 1 : 0;
      }

      // Move to the next column in the dummy matrix
      current_col++;
    }
  }

  return dummy_matrix;  // Return the resulting dummy matrix
}
