// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Flugbahn
NumericMatrix Flugbahn(const double v0, const double t, const double angle, const NumericVector target_vector, const double m, const double k, const int nb_return_values);
RcppExport SEXP _slvwagner_Flugbahn(SEXP v0SEXP, SEXP tSEXP, SEXP angleSEXP, SEXP target_vectorSEXP, SEXP mSEXP, SEXP kSEXP, SEXP nb_return_valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type v0(v0SEXP);
    Rcpp::traits::input_parameter< const double >::type t(tSEXP);
    Rcpp::traits::input_parameter< const double >::type angle(angleSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type target_vector(target_vectorSEXP);
    Rcpp::traits::input_parameter< const double >::type m(mSEXP);
    Rcpp::traits::input_parameter< const double >::type k(kSEXP);
    Rcpp::traits::input_parameter< const int >::type nb_return_values(nb_return_valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(Flugbahn(v0, t, angle, target_vector, m, k, nb_return_values));
    return rcpp_result_gen;
END_RCPP
}
// pendulum_motion
NumericMatrix pendulum_motion(const int L, const double delta_t, const double THETA_0, const double THETA_DOT_0, const double mu, const double calculation_stop, const int nb_return_values);
RcppExport SEXP _slvwagner_pendulum_motion(SEXP LSEXP, SEXP delta_tSEXP, SEXP THETA_0SEXP, SEXP THETA_DOT_0SEXP, SEXP muSEXP, SEXP calculation_stopSEXP, SEXP nb_return_valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type L(LSEXP);
    Rcpp::traits::input_parameter< const double >::type delta_t(delta_tSEXP);
    Rcpp::traits::input_parameter< const double >::type THETA_0(THETA_0SEXP);
    Rcpp::traits::input_parameter< const double >::type THETA_DOT_0(THETA_DOT_0SEXP);
    Rcpp::traits::input_parameter< const double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< const double >::type calculation_stop(calculation_stopSEXP);
    Rcpp::traits::input_parameter< const int >::type nb_return_values(nb_return_valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values));
    return rcpp_result_gen;
END_RCPP
}
// find_edges_2D
LogicalVector find_edges_2D(const LogicalVector c_search, const int n_col);
RcppExport SEXP _slvwagner_find_edges_2D(SEXP c_searchSEXP, SEXP n_colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalVector >::type c_search(c_searchSEXP);
    Rcpp::traits::input_parameter< const int >::type n_col(n_colSEXP);
    rcpp_result_gen = Rcpp::wrap(find_edges_2D(c_search, n_col));
    return rcpp_result_gen;
END_RCPP
}
// find_edges_3D
LogicalVector find_edges_3D(LogicalVector c_search, const IntegerVector n);
RcppExport SEXP _slvwagner_find_edges_3D(SEXP c_searchSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalVector >::type c_search(c_searchSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(find_edges_3D(c_search, n));
    return rcpp_result_gen;
END_RCPP
}
// f_ableitung
std::complex<double> f_ableitung(const std::complex<double> x);
RcppExport SEXP _slvwagner_f_ableitung(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::complex<double> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(f_ableitung(x));
    return rcpp_result_gen;
END_RCPP
}
// f
std::complex<double> f(const std::complex<double> x);
RcppExport SEXP _slvwagner_f(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::complex<double> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(f(x));
    return rcpp_result_gen;
END_RCPP
}
// f_vector
std::vector<std::complex<double>> f_vector(const std::vector<std::complex<double>> x);
RcppExport SEXP _slvwagner_f_vector(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(f_vector(x));
    return rcpp_result_gen;
END_RCPP
}
// f_ableitung_vector
std::vector<std::complex<double>> f_ableitung_vector(const std::vector<std::complex<double>> x);
RcppExport SEXP _slvwagner_f_ableitung_vector(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(f_ableitung_vector(x));
    return rcpp_result_gen;
END_RCPP
}
// function_calc_complex
std::complex<double> function_calc_complex(const std::complex<double> x, const std::vector <double> poly);
RcppExport SEXP _slvwagner_function_calc_complex(SEXP xSEXP, SEXP polySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::complex<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector <double> >::type poly(polySEXP);
    rcpp_result_gen = Rcpp::wrap(function_calc_complex(x, poly));
    return rcpp_result_gen;
END_RCPP
}
// vector_function_calc_complex
std::vector<std::complex<double>> vector_function_calc_complex(const std::vector<std::complex<double>> x, const std::vector <double> poly);
RcppExport SEXP _slvwagner_vector_function_calc_complex(SEXP xSEXP, SEXP polySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector <double> >::type poly(polySEXP);
    rcpp_result_gen = Rcpp::wrap(vector_function_calc_complex(x, poly));
    return rcpp_result_gen;
END_RCPP
}
// find_roots
int find_roots(const std::vector<std::complex<double>> x_n, const std::vector<std::complex<double>> roots, const int digits);
RcppExport SEXP _slvwagner_find_roots(SEXP x_nSEXP, SEXP rootsSEXP, SEXP digitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type x_n(x_nSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type roots(rootsSEXP);
    Rcpp::traits::input_parameter< const int >::type digits(digitsSEXP);
    rcpp_result_gen = Rcpp::wrap(find_roots(x_n, roots, digits));
    return rcpp_result_gen;
END_RCPP
}
// find_roots_v2
int find_roots_v2(const std::vector<std::complex<double>> x_n, const std::vector<std::complex<double>> roots, const int digits);
RcppExport SEXP _slvwagner_find_roots_v2(SEXP x_nSEXP, SEXP rootsSEXP, SEXP digitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type x_n(x_nSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type roots(rootsSEXP);
    Rcpp::traits::input_parameter< const int >::type digits(digitsSEXP);
    rcpp_result_gen = Rcpp::wrap(find_roots_v2(x_n, roots, digits));
    return rcpp_result_gen;
END_RCPP
}
// calc_it
DataFrame calc_it(std::vector<std::complex<double>> seeds, const std::vector <double> poly, const std::vector <double> poly_, const std::vector<std::complex<double>> poly_roots, const int c_calculation_depth, const int digits);
RcppExport SEXP _slvwagner_calc_it(SEXP seedsSEXP, SEXP polySEXP, SEXP poly_SEXP, SEXP poly_rootsSEXP, SEXP c_calculation_depthSEXP, SEXP digitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::complex<double>> >::type seeds(seedsSEXP);
    Rcpp::traits::input_parameter< const std::vector <double> >::type poly(polySEXP);
    Rcpp::traits::input_parameter< const std::vector <double> >::type poly_(poly_SEXP);
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type poly_roots(poly_rootsSEXP);
    Rcpp::traits::input_parameter< const int >::type c_calculation_depth(c_calculation_depthSEXP);
    Rcpp::traits::input_parameter< const int >::type digits(digitsSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_it(seeds, poly, poly_, poly_roots, c_calculation_depth, digits));
    return rcpp_result_gen;
END_RCPP
}
// calc_it_V2
DataFrame calc_it_V2(std::vector<std::complex<double>> seeds, const std::vector<std::complex<double>> poly_roots, const int c_calculation_depth, const int digits);
RcppExport SEXP _slvwagner_calc_it_V2(SEXP seedsSEXP, SEXP poly_rootsSEXP, SEXP c_calculation_depthSEXP, SEXP digitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::complex<double>> >::type seeds(seedsSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type poly_roots(poly_rootsSEXP);
    Rcpp::traits::input_parameter< const int >::type c_calculation_depth(c_calculation_depthSEXP);
    Rcpp::traits::input_parameter< const int >::type digits(digitsSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_it_V2(seeds, poly_roots, c_calculation_depth, digits));
    return rcpp_result_gen;
END_RCPP
}
// calc_it_V3
DataFrame calc_it_V3(const std::vector<std::complex<double>> seeds, const std::vector<std::complex<double>> poly_roots, const int c_calculation_depth, const int digits);
RcppExport SEXP _slvwagner_calc_it_V3(SEXP seedsSEXP, SEXP poly_rootsSEXP, SEXP c_calculation_depthSEXP, SEXP digitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type seeds(seedsSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::complex<double>> >::type poly_roots(poly_rootsSEXP);
    Rcpp::traits::input_parameter< const int >::type c_calculation_depth(c_calculation_depthSEXP);
    Rcpp::traits::input_parameter< const int >::type digits(digitsSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_it_V3(seeds, poly_roots, c_calculation_depth, digits));
    return rcpp_result_gen;
END_RCPP
}
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
    {"_slvwagner_Flugbahn", (DL_FUNC) &_slvwagner_Flugbahn, 7},
    {"_slvwagner_pendulum_motion", (DL_FUNC) &_slvwagner_pendulum_motion, 7},
    {"_slvwagner_find_edges_2D", (DL_FUNC) &_slvwagner_find_edges_2D, 2},
    {"_slvwagner_find_edges_3D", (DL_FUNC) &_slvwagner_find_edges_3D, 2},
    {"_slvwagner_f_ableitung", (DL_FUNC) &_slvwagner_f_ableitung, 1},
    {"_slvwagner_f", (DL_FUNC) &_slvwagner_f, 1},
    {"_slvwagner_f_vector", (DL_FUNC) &_slvwagner_f_vector, 1},
    {"_slvwagner_f_ableitung_vector", (DL_FUNC) &_slvwagner_f_ableitung_vector, 1},
    {"_slvwagner_function_calc_complex", (DL_FUNC) &_slvwagner_function_calc_complex, 2},
    {"_slvwagner_vector_function_calc_complex", (DL_FUNC) &_slvwagner_vector_function_calc_complex, 2},
    {"_slvwagner_find_roots", (DL_FUNC) &_slvwagner_find_roots, 3},
    {"_slvwagner_find_roots_v2", (DL_FUNC) &_slvwagner_find_roots_v2, 3},
    {"_slvwagner_calc_it", (DL_FUNC) &_slvwagner_calc_it, 6},
    {"_slvwagner_calc_it_V2", (DL_FUNC) &_slvwagner_calc_it_V2, 4},
    {"_slvwagner_calc_it_V3", (DL_FUNC) &_slvwagner_calc_it_V3, 4},
    {"_slvwagner_calc_roots_from_seeds_C", (DL_FUNC) &_slvwagner_calc_roots_from_seeds_C, 3},
    {"_slvwagner_expandGridComplex", (DL_FUNC) &_slvwagner_expandGridComplex, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_slvwagner(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
