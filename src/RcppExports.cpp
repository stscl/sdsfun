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
// GetJenksBreaks
NumericVector GetJenksBreaks(NumericVector inp_data, int n_classes, bool is_sorted);
RcppExport SEXP _sdsfun_GetJenksBreaks(SEXP inp_dataSEXP, SEXP n_classesSEXP, SEXP is_sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type inp_data(inp_dataSEXP);
    Rcpp::traits::input_parameter< int >::type n_classes(n_classesSEXP);
    Rcpp::traits::input_parameter< bool >::type is_sorted(is_sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(GetJenksBreaks(inp_data, n_classes, is_sorted));
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

static const R_CallMethodDef CallEntries[] = {
    {"_sdsfun_sdDisc", (DL_FUNC) &_sdsfun_sdDisc, 2},
    {"_sdsfun_equalDisc", (DL_FUNC) &_sdsfun_equalDisc, 2},
    {"_sdsfun_geometricDisc", (DL_FUNC) &_sdsfun_geometricDisc, 2},
    {"_sdsfun_quantileDisc", (DL_FUNC) &_sdsfun_quantileDisc, 2},
    {"_sdsfun_manualDisc", (DL_FUNC) &_sdsfun_manualDisc, 2},
    {"_sdsfun_naturalDisc", (DL_FUNC) &_sdsfun_naturalDisc, 3},
    {"_sdsfun_DummyVar", (DL_FUNC) &_sdsfun_DummyVar, 1},
    {"_sdsfun_DummyMat", (DL_FUNC) &_sdsfun_DummyMat, 1},
    {"_sdsfun_CalcFactorQ", (DL_FUNC) &_sdsfun_CalcFactorQ, 2},
    {"_sdsfun_GDFactorQ", (DL_FUNC) &_sdsfun_GDFactorQ, 2},
    {"_sdsfun_MI_vec", (DL_FUNC) &_sdsfun_MI_vec, 4},
    {"_sdsfun_GetJenksBreaks", (DL_FUNC) &_sdsfun_GetJenksBreaks, 3},
    {"_sdsfun_PrintGlobalMoranI", (DL_FUNC) &_sdsfun_PrintGlobalMoranI, 1},
    {"_sdsfun_RcppUnique", (DL_FUNC) &_sdsfun_RcppUnique, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_sdsfun(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
