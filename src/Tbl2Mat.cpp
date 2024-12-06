#include <vector>
#include <algorithm>
#include <unordered_map>
#include <tuple>
#include <limits>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List Tbl2Mat(const Rcpp::NumericMatrix& coords, const Rcpp::NumericVector& z_values) {
  // Extract x and y coordinates from the matrix
  std::vector<double> x_coords(coords.nrow());
  std::vector<double> y_coords(coords.nrow());
  for (int i = 0; i < coords.nrow(); ++i) {
    x_coords[i] = coords(i, 0);
    y_coords[i] = coords(i, 1);
  }

  // Find the unique x and y coordinates
  std::vector<double> unique_x = x_coords;
  std::vector<double> unique_y = y_coords;
  std::sort(unique_x.begin(), unique_x.end());
  std::sort(unique_y.begin(), unique_y.end());
  unique_x.erase(std::unique(unique_x.begin(), unique_x.end()), unique_x.end());
  unique_y.erase(std::unique(unique_y.begin(), unique_y.end()), unique_y.end());

  // Create a mapping from coordinates to matrix indices
  std::unordered_map<double, int> x_map, y_map;
  for (size_t i = 0; i < unique_x.size(); ++i) {
    x_map[unique_x[i]] = i;
  }
  for (size_t i = 0; i < unique_y.size(); ++i) {
    y_map[unique_y[i]] = i;
  }

  // Calculate the dimensions of the matrices
  int rows = unique_y.size();
  int cols = unique_x.size();

  // Initialize the matrices
  Rcpp::NumericMatrix z_matrix(rows, cols);
  Rcpp::NumericMatrix coords_matrix_x(rows, cols);
  Rcpp::NumericMatrix coords_matrix_y(rows, cols);

  // Fill the matrices
  for (int i = 0; i < coords.nrow(); ++i) {
    int x_idx = x_map[x_coords[i]];
    int y_idx = y_map[y_coords[i]];
    z_matrix(y_idx, x_idx) = z_values[i];
    coords_matrix_x(y_idx, x_idx) = x_coords[i];
    coords_matrix_y(y_idx, x_idx) = y_coords[i];
  }

  // Return the matrices as an Rcpp List
  return Rcpp::List::create(
    Rcpp::Named("z_attrs_matrix") = z_matrix,
    Rcpp::Named("x_coords_matrix") = coords_matrix_x,
    Rcpp::Named("y_coords_matrix") = coords_matrix_y
  );
}