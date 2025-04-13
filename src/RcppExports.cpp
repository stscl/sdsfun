// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// sdDisc
Rcpp::IntegerVector sdDisc(const arma::vec& x, double n);
RcppExport SEXP _sdsfun_sdDisc(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(sdDisc(x, n));
    return rcpp_result_gen;
END_RCPP
}
// equalDisc
Rcpp::IntegerVector equalDisc(const arma::vec& x, double n);
RcppExport SEXP _sdsfun_equalDisc(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(equalDisc(x, n));
    return rcpp_result_gen;
END_RCPP
}
// geometricDisc
Rcpp::IntegerVector geometricDisc(const arma::vec& x, double n);
RcppExport SEXP _sdsfun_geometricDisc(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(geometricDisc(x, n));
    return rcpp_result_gen;
END_RCPP
}
// quantileDisc
Rcpp::IntegerVector quantileDisc(const arma::vec& x, double n);
RcppExport SEXP _sdsfun_quantileDisc(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(quantileDisc(x, n));
    return rcpp_result_gen;
END_RCPP
}
// manualDisc
Rcpp::IntegerVector manualDisc(const arma::vec& x, arma::vec breakpoint);
RcppExport SEXP _sdsfun_manualDisc(SEXP xSEXP, SEXP breakpointSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type breakpoint(breakpointSEXP);
    rcpp_result_gen = Rcpp::wrap(manualDisc(x, breakpoint));
    return rcpp_result_gen;
END_RCPP
}
// ArmaJenksBreaks
arma::vec ArmaJenksBreaks(const arma::vec& inp_data, int n_classes, bool is_sorted);
RcppExport SEXP _sdsfun_ArmaJenksBreaks(SEXP inp_dataSEXP, SEXP n_classesSEXP, SEXP is_sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type inp_data(inp_dataSEXP);
    Rcpp::traits::input_parameter< int >::type n_classes(n_classesSEXP);
    Rcpp::traits::input_parameter< bool >::type is_sorted(is_sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(ArmaJenksBreaks(inp_data, n_classes, is_sorted));
    return rcpp_result_gen;
END_RCPP
}
// naturalDisc
Rcpp::IntegerVector naturalDisc(const arma::vec& x, int n, double sampleprob);
RcppExport SEXP _sdsfun_naturalDisc(SEXP xSEXP, SEXP nSEXP, SEXP sampleprobSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type sampleprob(sampleprobSEXP);
    rcpp_result_gen = Rcpp::wrap(naturalDisc(x, n, sampleprob));
    return rcpp_result_gen;
END_RCPP
}
// htDisc
Rcpp::IntegerVector htDisc(const arma::vec& x, double thr);
RcppExport SEXP _sdsfun_htDisc(SEXP xSEXP, SEXP thrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type thr(thrSEXP);
    rcpp_result_gen = Rcpp::wrap(htDisc(x, thr));
    return rcpp_result_gen;
END_RCPP
}
// DummyVar
Rcpp::IntegerMatrix DummyVar(Rcpp::IntegerVector x);
RcppExport SEXP _sdsfun_DummyVar(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(DummyVar(x));
    return rcpp_result_gen;
END_RCPP
}
// DummyMat
Rcpp::IntegerMatrix DummyMat(Rcpp::IntegerMatrix mat);
RcppExport SEXP _sdsfun_DummyMat(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(DummyMat(mat));
    return rcpp_result_gen;
END_RCPP
}
// CalcFactorQ
double CalcFactorQ(Rcpp::NumericVector y, Rcpp::IntegerVector h);
RcppExport SEXP _sdsfun_CalcFactorQ(SEXP ySEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcFactorQ(y, h));
    return rcpp_result_gen;
END_RCPP
}
// GDFactorQ
Rcpp::List GDFactorQ(Rcpp::NumericVector y, Rcpp::IntegerVector h);
RcppExport SEXP _sdsfun_GDFactorQ(SEXP ySEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(GDFactorQ(y, h));
    return rcpp_result_gen;
END_RCPP
}
// MI_vec
Rcpp::DataFrame MI_vec(arma::mat x, arma::mat W, std::string alternative, bool symmetrize);
RcppExport SEXP _sdsfun_MI_vec(SEXP xSEXP, SEXP WSEXP, SEXP alternativeSEXP, SEXP symmetrizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< std::string >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< bool >::type symmetrize(symmetrizeSEXP);
    rcpp_result_gen = Rcpp::wrap(MI_vec(x, W, alternative, symmetrize));
    return rcpp_result_gen;
END_RCPP
}
// RcppHClustGeoMat
arma::mat RcppHClustGeoMat(const arma::mat& D0, const arma::mat& D1, double alpha, bool scale, Rcpp::Nullable<Rcpp::NumericVector> wt_);
RcppExport SEXP _sdsfun_RcppHClustGeoMat(SEXP D0SEXP, SEXP D1SEXP, SEXP alphaSEXP, SEXP scaleSEXP, SEXP wt_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type D0(D0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type D1(D1SEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< bool >::type scale(scaleSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::NumericVector> >::type wt_(wt_SEXP);
    rcpp_result_gen = Rcpp::wrap(RcppHClustGeoMat(D0, D1, alpha, scale, wt_));
    return rcpp_result_gen;
END_RCPP
}
// RcppJenksBreaks
Rcpp::NumericVector RcppJenksBreaks(const Rcpp::NumericVector& x, int n_classes, bool is_sorted);
RcppExport SEXP _sdsfun_RcppJenksBreaks(SEXP xSEXP, SEXP n_classesSEXP, SEXP is_sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n_classes(n_classesSEXP);
    Rcpp::traits::input_parameter< bool >::type is_sorted(is_sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppJenksBreaks(x, n_classes, is_sorted));
    return rcpp_result_gen;
END_RCPP
}
// LinearRegression
Rcpp::NumericVector LinearRegression(const Rcpp::NumericVector& y, const Rcpp::NumericMatrix& X);
RcppExport SEXP _sdsfun_LinearRegression(SEXP ySEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(LinearRegression(y, X));
    return rcpp_result_gen;
END_RCPP
}
// LinearTrendRM
Rcpp::NumericVector LinearTrendRM(const Rcpp::NumericVector& y, const Rcpp::NumericMatrix& X);
RcppExport SEXP _sdsfun_LinearTrendRM(SEXP ySEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(LinearTrendRM(y, X));
    return rcpp_result_gen;
END_RCPP
}
// PrintGlobalMoranI
Rcpp::DataFrame PrintGlobalMoranI(Rcpp::DataFrame df);
RcppExport SEXP _sdsfun_PrintGlobalMoranI(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(PrintGlobalMoranI(df));
    return rcpp_result_gen;
END_RCPP
}
// RcppUnique
Rcpp::IntegerVector RcppUnique(Rcpp::IntegerVector x);
RcppExport SEXP _sdsfun_RcppUnique(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppUnique(x));
    return rcpp_result_gen;
END_RCPP
}
// CalcSPADEPSD
double CalcSPADEPSD(Rcpp::NumericVector y, Rcpp::IntegerVector h, Rcpp::NumericMatrix wt);
RcppExport SEXP _sdsfun_CalcSPADEPSD(SEXP ySEXP, SEXP hSEXP, SEXP wtSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type h(hSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type wt(wtSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcSPADEPSD(y, h, wt));
    return rcpp_result_gen;
END_RCPP
}
// RcppSpatialVariance
double RcppSpatialVariance(Rcpp::NumericVector x, Rcpp::NumericMatrix wt);
RcppExport SEXP _sdsfun_RcppSpatialVariance(SEXP xSEXP, SEXP wtSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type wt(wtSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppSpatialVariance(x, wt));
    return rcpp_result_gen;
END_RCPP
}
// Tbl2Mat
Rcpp::List Tbl2Mat(const Rcpp::NumericMatrix& coords, const Rcpp::NumericVector& z_values);
RcppExport SEXP _sdsfun_Tbl2Mat(SEXP coordsSEXP, SEXP z_valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type z_values(z_valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(Tbl2Mat(coords, z_values));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sdsfun_sdDisc", (DL_FUNC) &_sdsfun_sdDisc, 2},
    {"_sdsfun_equalDisc", (DL_FUNC) &_sdsfun_equalDisc, 2},
    {"_sdsfun_geometricDisc", (DL_FUNC) &_sdsfun_geometricDisc, 2},
    {"_sdsfun_quantileDisc", (DL_FUNC) &_sdsfun_quantileDisc, 2},
    {"_sdsfun_manualDisc", (DL_FUNC) &_sdsfun_manualDisc, 2},
    {"_sdsfun_ArmaJenksBreaks", (DL_FUNC) &_sdsfun_ArmaJenksBreaks, 3},
    {"_sdsfun_naturalDisc", (DL_FUNC) &_sdsfun_naturalDisc, 3},
    {"_sdsfun_htDisc", (DL_FUNC) &_sdsfun_htDisc, 2},
    {"_sdsfun_DummyVar", (DL_FUNC) &_sdsfun_DummyVar, 1},
    {"_sdsfun_DummyMat", (DL_FUNC) &_sdsfun_DummyMat, 1},
    {"_sdsfun_CalcFactorQ", (DL_FUNC) &_sdsfun_CalcFactorQ, 2},
    {"_sdsfun_GDFactorQ", (DL_FUNC) &_sdsfun_GDFactorQ, 2},
    {"_sdsfun_MI_vec", (DL_FUNC) &_sdsfun_MI_vec, 4},
    {"_sdsfun_RcppHClustGeoMat", (DL_FUNC) &_sdsfun_RcppHClustGeoMat, 5},
    {"_sdsfun_RcppJenksBreaks", (DL_FUNC) &_sdsfun_RcppJenksBreaks, 3},
    {"_sdsfun_LinearRegression", (DL_FUNC) &_sdsfun_LinearRegression, 2},
    {"_sdsfun_LinearTrendRM", (DL_FUNC) &_sdsfun_LinearTrendRM, 2},
    {"_sdsfun_PrintGlobalMoranI", (DL_FUNC) &_sdsfun_PrintGlobalMoranI, 1},
    {"_sdsfun_RcppUnique", (DL_FUNC) &_sdsfun_RcppUnique, 1},
    {"_sdsfun_CalcSPADEPSD", (DL_FUNC) &_sdsfun_CalcSPADEPSD, 3},
    {"_sdsfun_RcppSpatialVariance", (DL_FUNC) &_sdsfun_RcppSpatialVariance, 2},
    {"_sdsfun_Tbl2Mat", (DL_FUNC) &_sdsfun_Tbl2Mat, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_sdsfun(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
