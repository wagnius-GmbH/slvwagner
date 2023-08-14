// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// calc_roots_from_seeds_C
List calc_roots_from_seeds_C(ComplexVector seeds, ComplexVector roots, NumericVector poly);
RcppExport SEXP _slvwagner_calc_roots_from_seeds_C(SEXP seedsSEXP, SEXP rootsSEXP, SEXP polySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ComplexVector >::type seeds(seedsSEXP);
    Rcpp::traits::input_parameter< ComplexVector >::type roots(rootsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type poly(polySEXP);
    rcpp_result_gen = Rcpp::wrap(calc_roots_from_seeds_C(seeds, roots, poly));
    return rcpp_result_gen;
END_RCPP
}
// expandGridComplex
ComplexVector expandGridComplex(ComplexVector x, double epsilon);
RcppExport SEXP _slvwagner_expandGridComplex(SEXP xSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ComplexVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    rcpp_result_gen = Rcpp::wrap(expandGridComplex(x, epsilon));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_slvwagner_calc_roots_from_seeds_C", (DL_FUNC) &_slvwagner_calc_roots_from_seeds_C, 3},
    {"_slvwagner_expandGridComplex", (DL_FUNC) &_slvwagner_expandGridComplex, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_slvwagner(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
