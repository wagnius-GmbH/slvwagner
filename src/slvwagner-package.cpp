#include <Rcpp.h>
#include <string>
#include <vector>
#include <complex>
#include <Rmath.h>
#include <R.h>

using namespace Rcpp;
using namespace std;

//////////////////////////////////////////////////////////////////////
class class_roots_from_seeds {

  public:
  ComplexVector roots;
  ComplexVector roots_complex_conjugated;
  ComplexVector seeds;
  NumericVector poly;
  NumericVector poly_;
  int digits;
  int c_calculation_depth;
  IntegerVector roots_from_seeds;

  //Constructor
  class_roots_from_seeds(ComplexVector seeds_, ComplexVector roots_, NumericVector polynomial ){
    digits = 8;
    seeds= seeds_;
    roots = roots_;
    poly = polynomial;
    computeDerivative_C(); //calc poly_ Derivative of poly
    createComplexConjugated();
    deleteComplexConjugated();
    c_calculation_depth = 1000;
    //Result
    IntegerVector roots_from_seeds(seeds_.size());

  };

  void computeDerivative_C() {
    // Calculate the derivative of the polynomial
    NumericVector derivative;
    for (int i = 1; i < int(poly.size()); ++i) {
        derivative.push_back(poly[i] * i);
    }
    poly_ = derivative;
    }

  void createComplexConjugated() {
    if (roots_complex_conjugated.size() != 0)
      {
      for (int ii = roots_complex_conjugated.size() - 1; ii > -1; ii--)
        {
          roots_complex_conjugated.erase(roots_complex_conjugated.begin() + ii);
        }
      }
    for (int ii = 0; ii < roots.size(); ii++)
      {
          roots_complex_conjugated.push_back(Rcomplex{roots[ii].r, roots[ii].i});
      }
  }
  void deleteComplexConjugated() {
    // Delete if not only complex conjugated
    for (int ii = roots_complex_conjugated.size() - 1; ii > -1; ii--)
      {
        Rcomplex Point = roots_complex_conjugated[ii];
          if (Point.r == 0) {
            roots_complex_conjugated.erase(ii);
          }
      }
  }

  void calc_roots_from_seed
    (
        int ii,
        const std::function<std::complex<double>(const std::complex<double>&)>& function_calc_complex,
        const std::function<std::complex<double>(const std::complex<double>&)>& function_calc_complex_
    )
    {
        std::complex<double> x_n[2];

        double distance;
        int poly_size = roots.size();
        int poly_size_ = roots_complex_conjugated.size();

        // How accurate shall the root be found
        double test = R_pow(10, -digits);
        x_n[0] = std::complex(seeds[ii].r, seeds[ii].i);
        x_n[1] = x_n[0] - function_calc_complex(x_n[0]) / function_calc_complex_(x_n[0]);

        bool run = true;
        int jj = 0;

        bool found_root = false;
        while (run) {
            int kk = 0;
            // Newtonian optimisation algorithm
            x_n[1] = x_n[0] - (function_calc_complex(x_n[0]) / function_calc_complex_(x_n[0]));
            x_n[0] = x_n[1];
            // Find complex conjugated root
            for (int kk = 0; kk < poly_size_; kk++) {
                // If distance is smaller than the defined threshold => root has been found
                distance = abs(x_n[1] - std::complex<double>(double(roots_complex_conjugated[kk].r), double(roots_complex_conjugated[kk].i)));
                if (distance < test) {
                    roots_from_seeds[ii] = kk;
                    found_root = true;
                    break;
                }
            }
            // Find complex root
            for (kk = 0; kk < poly_size; kk++) {
                // If distance is smaller than the defined threshold => root has been found
                distance = abs(x_n[1] - std::complex<double>(double(roots_complex_conjugated[kk].r), double(roots_complex_conjugated[kk].i)));
                if (distance < test) {
                    roots_from_seeds[ii] = kk;
                    found_root = true;
                    break;
                }
            }
            jj++;
            // Stop root found
            if (found_root == true) {
                run = false;
            }
            else if (jj == c_calculation_depth) { // Stop Root not found
                roots_from_seeds[ii] = 1000;
                run = false;
            }
        }
    }

    void parallel_lambda_calc_roots_from_seeds() {
      //#pragma omp parallel for schedule(dynamic, 1)
      int seed_size = int(seeds.size());
      //#pragma omp parallel for
      for (int ii = 0; ii < seed_size; ++ii) {
        calc_roots_from_seed(ii,
            [&](const std::complex<double>& x) {
                // Implementation of function_calc_complex
                std::complex<double> c_sum(0, 0);
                for (unsigned int i = 0; i < poly.size(); i++) {
                    c_sum += poly[i] * std::pow(x, i);
                }
                return c_sum;
            },
            [&](const std::complex<double>& x) {
                // Implementation of function_calc_complex_
                std::complex<double> c_sum(0, 0);
                for (unsigned int i = 0; i < poly_.size(); i++) {
                    c_sum += poly_[i] * std::pow(x, i);
                }
                return c_sum;
            });
        }
    }
};


//////////////////////////////////////////////
//' calc_roots_from_seeds_C
//' @name calc_roots_from_seeds_C
//' @title calc_roots_from_seeds_C
//' @description Create Student data frame.
//' @param seeds seeds
//' @param roots roots from polynom equation
//' @param poly polynomial coefficients
//' @examples
//' library(Rcpp)
//' df <- expand.grid(re = seq(0,10,0.1), im = seq(-10,0,0.1))
//' seeds <- complex(df$re,df$im)
//' calc_roots_from_seeds_C(seeds, c(0.2,2.5,3+1i), 1:10)
//' @export

// [[Rcpp::export]]
List calc_roots_from_seeds_C(ComplexVector seeds, ComplexVector roots, NumericVector poly){
  class_roots_from_seeds object(seeds, roots, poly);
  //object.parallel_lambda_calc_roots_from_seeds();
  List df = List::create(Named("roots") = object.roots,
                         Named("roots_complex_conjugated") = object.roots_complex_conjugated,
                         Named("Seeds") = object.seeds,
                         Named("poly") = object.poly,
                         Named("poly_") = object.poly_
                           );
  return df;
}

vector<complex<double>> expandGridComplexRowMajorOrder(const vector<double>& realVals, const vector<double>& imagVals, double epsilon = 1e-10) {
  vector<complex<double>> result;
  for (int ii = int(imagVals.size()) - 1; ii > -1; ii--)
  {
    for (int jj = 0; jj < (int)realVals.size(); jj++)
    {
      double realPart = realVals[jj];
      double imagPart = imagVals[ii];
      // Rounding of imaginary part of complex number
      if (std::abs(imagPart) < epsilon) {
        imagPart = 0.0; // Consider it as zero
      }
      result.push_back(std::complex<double>(realPart, imagPart));
    }
  }
  return result;
}


/*** R
library(Rcpp)
df <- expand.grid(re = seq(0,10,0.1),
                 im = seq(-10,0,0.1))
seeds <- complex(df$re,df$im)
calc_roots_from_seeds_C(seeds, c(0.2,2.5,3+1i), 1:10)

*/
