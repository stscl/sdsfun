#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Ward initialization function
arma::mat RcppWardInit(const arma::mat& D, const arma::vec& wt) {
  int n = D.n_rows;
  if (wt.n_elem == 0) {
    return arma::pow(D, 2) / (2.0 * n);
  } else {
    arma::mat delta_mat = arma::pow(D, 2);
    delta_mat.each_col() %= wt;  // Row-wise scaling
    delta_mat.each_row() %= wt.t();  // Column-wise scaling

    arma::mat S = arma::repmat(wt, 1, n);
    delta_mat /= (S + S.t());
    return delta_mat;
  }
}

// [[Rcpp::export(rng = false)]]
arma::mat RcppHClustGeoMat(const arma::mat& D0,
                           const arma::mat& D1,
                           double alpha,
                           bool scale = true,
                           Rcpp::Nullable<Rcpp::NumericVector> wt_ = R_NilValue) {
  // Check inputs
  if (D0.n_rows != D0.n_cols || (!D1.is_empty() && D1.n_rows != D1.n_cols)) {
    Rcpp::stop("Both D0 and D1 must be square matrices.");
  }

  if (!D1.is_empty() && D0.n_rows != D1.n_rows) {
    Rcpp::stop("D0 and D1 must have the same dimensions.");
  }

  if (alpha < 0 || alpha > 1) {
    Rcpp::stop("Alpha must be in the range [0, 1].");
  }

  int n = D0.n_rows;
  if (n < 2) {
    Rcpp::stop("Must have at least 2 objects to cluster.");
  }

  arma::vec wt;
  if (wt_.isNotNull()) {
    wt = Rcpp::as<arma::vec>(wt_);
    if (wt.n_elem != D0.n_rows) {
      Rcpp::stop("Weight vector length must match the number of objects.");
    }
  } else {
    wt = arma::vec(n, arma::fill::ones); // Default weights are equal
  }

  // Compute the maximum elements of D0 and D1
  double max_D0 = arma::max(arma::max(D0));
  double max_D1 = arma::max(arma::max(D1));

  // Normalize dissimilarity matrices if scaling is enabled
  // arma::mat D0_scaled = D0;
  // arma::mat D1_scaled = D1;
  // if (scale) {
  //   D0_scaled /= arma::max(arma::max(D0));
  //   if (!D1.is_empty()) {
  //     D1_scaled /= arma::max(arma::max(D1));
  //   }
  // }

  // Normalize dissimilarity matrices if scaling is enabled and max element is non-zero
  arma::mat D0_scaled = D0;
  arma::mat D1_scaled = D1;
  if (scale) {
    if (max_D0 > 0) {
      D0_scaled /= max_D0;
    }
    if (max_D1 > 0 && !D1.is_empty()) {
      D1_scaled /= max_D1;
    }
  }

  // Compute initial Ward distances
  arma::mat delta0 = RcppWardInit(D0_scaled, wt);
  arma::mat delta1 = !D1.is_empty() ? RcppWardInit(D1_scaled, wt) : arma::mat(n, n, arma::fill::zeros);

  // Combine dissimilarities based on alpha
  arma::mat delta_mat = (1 - alpha) * delta0 + alpha * delta1;

  // Return dissimilarity matrix
  return delta_mat;
}
