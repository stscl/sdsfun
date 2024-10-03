#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Function to calculate p-value based on z and alternative hypothesis
double pfunc(double z, std::string alternative) {
  if (alternative == "greater") {
    return R::pnorm(z, 0.0, 1.0, false, false);
  } else if (alternative == "less") {
    return R::pnorm(z, 0.0, 1.0, true, false);
  } else { // "two.sided"
    return 2 * R::pnorm(std::abs(z), 0.0, 1.0, false, false);
  }
}

// [[Rcpp::export]]
Rcpp::DataFrame MI_vec(arma::mat x, arma::mat W,
                       std::string alternative = "greater",
                       bool symmetrize = false) {
  if (symmetrize) {
    W = 0.5 * (W + W.t());
  }

  int nx = x.n_cols;
  int n = W.n_rows;
  double df = n - 1;
  double S0 = arma::accu(W);
  double S1 = arma::accu(arma::square(W) + W % W.t());
  double S2 = arma::accu(arma::square(arma::sum(W, 0)) + arma::square(arma::sum(W, 1).t()));

  // projection matrix M
  arma::mat M = arma::eye(n, n) - arma::ones(n, n) / n;
  arma::mat MWM = M * W * M;

  // Output DataFrame
  Rcpp::DataFrame out;
  Rcpp::NumericVector I(nx), EI(nx), VarI(nx), zI(nx), pI(nx);
  Rcpp::CharacterVector stars(nx);

  for (int i = 0; i < nx; ++i) {
    arma::vec xi = x.col(i);

    // observed
    I[i] = (n / S0) * as_scalar(xi.t() * MWM * xi) / as_scalar(xi.t() * M * xi);
    // expected
    EI[i] = -1 / df;
    // variance (normality assumption)
    VarI[i] = ((n * n * S1 - n * S2 + 3 * S0 * S0) / (S0 * S0 * (n * n - 1))) - EI[i] * EI[i];
    // test statistic
    zI[i] = (I[i] - EI[i]) / std::sqrt(VarI[i]);
    // pI
    pI[i] = pfunc(zI[i], alternative);
  }

  out = Rcpp::DataFrame::create(Rcpp::Named("MoranI") = I,
                                Rcpp::Named("EI") = EI,
                                Rcpp::Named("VarI") = VarI,
                                Rcpp::Named("ZI") = zI,
                                Rcpp::Named("PI") = pI);

  return out;
}
