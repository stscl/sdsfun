#include <vector>
#include "Entropy.h"
// 'Rcpp.h' should not be included and correct to include only 'RcppArmadillo.h'.
// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double RcppEntropy_Disc(const Rcpp::NumericVector& vec,
                        double base = 10, bool NA_rm = false){
  std::vector<double> v1 = Rcpp::as<std::vector<double>>(vec);
  return CppEntropy_Disc(v1,base,NA_rm);
};

// [[Rcpp::export]]
double RcppJoinEntropy_Disc(const Rcpp::NumericMatrix& mat,
                            const Rcpp::IntegerVector& columns,
                            double base = 10,
                            bool NA_rm = false){
  int numRows = mat.nrow();
  int numCols = mat.ncol();
  // Convert Rcpp NumericMatrix to std::vector<std::vector<double>>
  std::vector<std::vector<double>> cppMat(numRows, std::vector<double>(numCols));
  for (int i = 0; i < numRows; ++i) {
    for (int j = 0; j < numCols; ++j) {
      cppMat[i][j] = mat(i, j);
    }
  }

  // Convert Rcpp IntegerVector to std::vector<int>
  std::vector<int> col_std = Rcpp::as<std::vector<int>>(columns);
  for (size_t i = 0; i < col_std.size(); ++i) {
    if (col_std[i] < 1 || col_std[i] > numCols) {
      Rcpp::stop("Each index in 'columns' must be between 1 and %d (inclusive).", numCols);
    }
    col_std[i] -= 1;
  }

  return CppJoinEntropy_Disc(cppMat,col_std,base,NA_rm);
};

// [[Rcpp::export]]
double RcppMutualInformation_Disc(const Rcpp::NumericMatrix& mat,
                                  const Rcpp::IntegerVector& columns1,
                                  const Rcpp::IntegerVector& columns2,
                                  double base = 10,
                                  bool NA_rm = false){
  int numRows = mat.nrow();
  int numCols = mat.ncol();

  // Convert Rcpp NumericMatrix to std::vector<std::vector<double>>
  std::vector<std::vector<double>> cppMat(numRows, std::vector<double>(numCols));
  for (int i = 0; i < numRows; ++i) {
    for (int j = 0; j < numCols; ++j) {
      cppMat[i][j] = mat(i, j);
    }
  }

  // Convert Rcpp IntegerVector to std::vector<int>
  std::vector<int> col1 = Rcpp::as<std::vector<int>>(columns1);
  for (size_t i = 0; i < col1.size(); ++i) {
    if (col1[i] < 1 || col1[i] > numCols) {
      Rcpp::stop("Each index in 'columns1' must be between 1 and %d (inclusive).", numCols);
    }
    col1[i] -= 1;
  }
  std::vector<int> col2 = Rcpp::as<std::vector<int>>(columns2);
  for (size_t i = 0; i < col2.size(); ++i) {
    if (col2[i] < 1 || col2[i] > numCols) {
      Rcpp::stop("Each index in 'columns2' must be between 1 and %d (inclusive).", numCols);
    }
    col2[i] -= 1;
  }

  return CppMutualInformation_Disc(cppMat,col1,col2,base,NA_rm);
}

// [[Rcpp::export]]
double RcppConditionalEntropy_Disc(const Rcpp::NumericMatrix& mat,
                                   const Rcpp::IntegerVector& target_columns,
                                   const Rcpp::IntegerVector& conditional_columns,
                                   double base = 10,
                                   bool NA_rm = false){
  int numRows = mat.nrow();
  int numCols = mat.ncol();

  // Convert Rcpp NumericMatrix to std::vector<std::vector<double>>
  std::vector<std::vector<double>> cppMat(numRows, std::vector<double>(numCols));
  for (int i = 0; i < numRows; ++i) {
    for (int j = 0; j < numCols; ++j) {
      cppMat[i][j] = mat(i, j);
    }
  }

  // Convert Rcpp IntegerVector to std::vector<int>
  std::vector<int> col1 = Rcpp::as<std::vector<int>>(target_columns);
  for (size_t i = 0; i < col1.size(); ++i) {
    if (col1[i] < 1 || col1[i] > numCols) {
      Rcpp::stop("Each index in 'target_columns' must be between 1 and %d (inclusive).", numCols);
    }
    col1[i] -= 1;
  }
  std::vector<int> col2 = Rcpp::as<std::vector<int>>(conditional_columns);
  for (size_t i = 0; i < col2.size(); ++i) {
    if (col2[i] < 1 || col2[i] > numCols) {
      Rcpp::stop("Each index in 'conditional_columns' must be between 1 and %d (inclusive).", numCols);
    }
    col2[i] -= 1;
  }

  return CppConditionalEntropy_Disc(cppMat,col1,col2,base,NA_rm);
}
