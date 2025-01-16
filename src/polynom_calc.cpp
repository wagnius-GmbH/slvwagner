#include <Rcpp.h>
#include <Rmath.h>
#include <complex.h>

#include <stdlib.h>

#include <vector>
#include <complex>
#include <cmath>

using namespace Rcpp;

//////////////////////////////////////////////
//' f_ableitung
//' @name f_ableitung
//' @title f_ableitung
//' @description Berechntet die Funktionswerte von f(x) = y = -1 + 2*x + 5*x^4.
//' @usage f_ableitung(x)
//' @param x vector
//' @examples f_ableitung(1+2i)
//' @export
/* this fixes it */
// [[Rcpp::export]]
 std::complex<double> f_ableitung(const std::complex<double> x)
 {
   return((std::complex<double>) - 1 + (std::complex<double>)2 * x + (std::complex<double>)5 * pow(x, (int)4));
 }


//////////////////////////////////////////////
//' f
//' @name f
//' @title f
//' @description Berechntet die Funktionswerte der Funktion f(x) = y = -0.2 - x + x^2 + x^5.
//' @usage f(x)
//' @param x vector
//' @examples f(1+2i)
//' @export
/* this fixes it */
// [[Rcpp::export]]
std::complex<double> f(const std::complex<double> x)
{
  return((std::complex<double>) - 0.2 - x + pow(x, (int)2) + pow(x, (int)5));
}


//////////////////////////////////////////////
//' f_vector
//' @name f_vector
//' @title f_vector
//' @description Berechntet die Funktionswerte eines Polynoms.
//' @usage f_vector(x)
//' @param x vector
//' @examples f_vector(c(1+2i,-0.2-0.8i))
//' @export
/* this fixes it */
// [[Rcpp::export]]
 std::vector<std::complex<double>> f_vector (const std::vector<std::complex<double>> x)
 {
   std::vector<std::complex<double>> result(x.size());
   for (long long unsigned int  ii = 0; ii < x.size(); ii++)
   {
     result[ii] = f(x[ii]);
   }
   return(result);
 }


//////////////////////////////////////////////
//' f_ableitung_vector
//' @name f_ableitung_vector
//' @title f_ableitung_vector
//' @description Berechntet die Funktionswerte eines Polynoms.
//' @usage f_ableitung_vector(x)
//' @param x vector
//' @examples f_ableitung_vector(c(1+2i,-0.2-0.8i))
//' @export
/* this fixes it */
// [[Rcpp::export]]
std::vector<std::complex<double>> f_ableitung_vector(const std::vector<std::complex<double>> x)
{
  std::vector<std::complex<double>> result(x.size());
  for (long long unsigned int ii = 0; ii < x.size(); ii++)
  {
    result[ii] = f_ableitung(x[ii]);
  }

  return(result);
}


///////////////////////////////////////////////
//'
//' @name function_calc_complex
//' @title function_calc_complex
//' @description Berechntet die Funktionswerte eines Polynoms.
//' @usage function_calc_complex(x,poly)
//' @param x vector
//' @param poly polynom vector
//' @examples function_calc_complex(1+2i,
//' @examples polynom::polynomial(coef = c(-0.2, -1.0, 1.0, 0.0, 0.0, 1.0)))|>plot()
//' @export
/* this fixes it */
// [[Rcpp::export]]
std::complex<double> function_calc_complex(
    const std::complex<double> x,
  const std::vector <double> poly
)
  {
  std::vector<std::complex<double>> result(poly.size());
  std::complex<double> c_sum(0, 0);
  for (long long unsigned int i = 0; i < poly.size(); i++)
  {
    result[i] = poly[i] * std::pow(x, i);
    c_sum += result[i];
  }
  return(c_sum);
}


///////////////////////////////////////////////
//'
//' @name vector_function_calc_complex
//' @title vector_function_calc_complex
//' @description Berechntet die Funktionswerte eines Polynoms für gegebenen Vektor.
//' @usage vector_function_calc_complex(x,poly)
//' @param x vector
//' @param poly polynom vector
//' @examples vector_function_calc_complex(-6:6,
//' @examples polynom::polynomial(coef = c(-0.2, -1.0, 1.0, 0.0, 0.0, 1.0)))|>plot()
//' @export
/* this fixes it */
// [[Rcpp::export]]
std::vector<std::complex<double>> vector_function_calc_complex
(
    const std::vector<std::complex<double>> x,
    const std::vector <double> poly
)
{
  std::vector<std::complex<double>> result(x.size());
  for (long long unsigned int i = 0; i < x.size(); i++)
  {
    result[i] = function_calc_complex(x[i],poly);
  }
  return(result);
}


///////////////////////////////////////////////
//'
//' find roots
//' @name find_roots
//' @title find_roots
//' @description finds roots in complex plane.
//' @usage find_roots(x_n,roots,digits)
//' @param x_n vector
//' @param roots vector
//' @param digits number of significant digits till calculation stops.
//' @examples
//' #######
//' # seeds
//' seeds <- expand.grid(seq(-1,1, length.out = 20),
//'                       seq(-1,1, length.out = 20))
//' seeds <- complex(real = seeds$Var1, imaginary = seeds$Var2)
//'
//' f <- function(x) 1 - x + x^2 + x^5
//' f_ableitung <- function(x) -1 + 2*x + 5*x^4
//'
//' f1 <- polynom::polynomial(coef = c(-0.2,-1,1,0,0,1))
//'
//' #######
//' x_n <- seeds|>
//'   lapply(function(x){
//'   x_n <- 1:100
//'   x_n[1] <- x
//'   for( i in 2:length(x_n)) {
//'     x_n[i] <- x_n[i-1]-(f(x_n[i-1])/f_ableitung(x_n[i-1]))
//'   }
//' return(x_n)
//' })
//' x_n[[1]]|>find_roots(summary(f1)$zeros,6)
//' @export
/* this fixes it */
// [[Rcpp::export]]
int find_roots
(
   const std::vector<std::complex<double>> x_n,
   const std::vector<std::complex<double>> roots,
   const int digits)
{
 double x_n_Re = 0; double x_n_Im = 0;
 double roots_Re = 0; double roots_Im = 0;
 int root_size = (int)roots.size();
 int x_n_size = (int)x_n.size();
 int jj = 0;

 for (int ii = 0; ii < x_n_size; ii++)
 {
   for (jj = 0 ;jj < root_size; jj++)
   {
     x_n_Re   = round(std::real(x_n[ii]    * (double)digits)) / (double)digits;
     x_n_Im   = round(std::imag(x_n[ii]    * (double)digits)) / (double)digits;
     roots_Re = round(std::real(roots[jj]) * (double)digits)  / (double)digits;
     roots_Im = round(std::imag(roots[jj]) * (double)digits)  / (double)digits;

     if ((x_n_Re == roots_Re) & (x_n_Im == roots_Im)) {
       ii = x_n_size;
       break;
     }
   }
 }
 return((int)jj+1);
}

///////////////////////////////////////////////
//' find_roots_v2
//'
//' @name find_roots_v2
//' @title find_roots_v2
//' @description finds roots in complex plane.
//' @usage find_roots_v2(x_n,roots,digits)
//' @param x_n vector
//' @param roots vector
//' @param digits number of significant digits till calculation stops.
//' @examples
//' #######
//' # seeds
//' seeds <- expand.grid(seq(-1,1, length.out = 20),
//'                       seq(-1,1, length.out = 20))
//' seeds <- complex(real = seeds$Var1, imaginary = seeds$Var2)
//'
//' f <- function(x) 1 - x + x^2 + x^5
//' f_ableitung <- function(x) -1 + 2*x + 5*x^4
//'
//' f1 <- polynom::polynomial(coef = c(-0.2,-1,1,0,0,1))
//'
//' #######
//' x_n <- seeds|>
//'   lapply(function(x){
//'   x_n <- 1:100
//'   x_n[1] <- x
//'   for( i in 2:length(x_n)) {
//'     x_n[i] <- x_n[i-1]-(f(x_n[i-1])/f_ableitung(x_n[i-1]))
//'   }
//' return(x_n)
//' })
//' x_n[[1]]|>find_roots_v2(summary(f1)$zeros,6)
//' @export
/* this fixes it */
// [[Rcpp::export]]
int find_roots_v2
(
    const std::vector<std::complex<double>> x_n,
    const std::vector<std::complex<double>> roots,
    const int digits
)
{
  double distance = 0;
  double deviation = pow(10, -digits);

  int ii = 0;
  int jj = 0;
  int root_size = (int)roots.size();
  int x_n_size = (int)x_n.size();

  for (ii = 0; ii < x_n_size; ii++)
  {
    for (jj = 1; jj < root_size; jj++)
    {
      distance =(double)abs(x_n[ii] - roots[jj]);
      if (distance < deviation) {
        ii = x_n_size;
        break;
      }
    }
  }
  return(jj + 1);
}

//////////////////////////////////////////////
//'
//' @name calc_it
//' @title calc_it
//' @description Berechntet die Funktionswerte eines Polynoms für gegebenen Vektor.
//' @usage calc_it(seeds, poly, poly_, poly_roots, c_calculation_depth, digits)
//' @param seeds complex vector
//' @param poly numeric vector  # polynom coefficients
//' @param poly_ numeric vector # derivative coefficients of the polynom => deriv(poly)
//' @param poly_roots complex roots vector # roots of the polynom => solve(poly)
//' @param c_calculation_depth #how many iterrations steps (depth of calculation)
//' @param digits how many decimal digit till calculation stop
//' @examples polynom <-polynom::polynomial(coef = c(-0.2,-1,1,0,0,1))
//' @examples roots <- solve(polynom)
//' @examples seeds <- expand.grid(seq(-2,2, length.out = 200),seq(-2,2, length.out = 200))
//' @examples seeds <- complex(real = seeds$Var1, imaginary = seeds$Var2)
//' @examples calc_it(seeds, polynom, deriv(polynom), roots,100,6)
//' @export
/* this fixes it */
// [[Rcpp::export]]
DataFrame calc_it
(
   std::vector<std::complex<double>> seeds,
   const std::vector <double> poly,
   const std::vector <double> poly_,
   const std::vector<std::complex<double>> poly_roots,
   const int c_calculation_depth,
   const int digits
)
{
  std::vector<std::complex<double>> x_n(c_calculation_depth);
  NumericVector roots (seeds.size(),0);

  for (long long unsigned int ii = 0; ii < seeds.size(); ii++)
  {
   x_n[0] = seeds[ii];
   for (int jj = 1; jj < c_calculation_depth; jj++)
   {
     x_n[jj] = x_n[jj - 1] - (function_calc_complex(x_n[jj - 1], poly) / function_calc_complex(x_n[jj - 1], poly_));
   }
   roots[ii] = find_roots(x_n, poly_roots, digits);
  }
  // When giving names to columns
  DataFrame df = DataFrame::create( Named("seeds") = seeds , _["roots"] = roots );
  return(df);
}



//////////////////////////////////////////////
//'
//' @name calc_it_V2
//' @title calc_it_V2
//' @description Berechntet die Funktionswerte eines Polynoms für gegebenen Vektor.
//' @usage calc_it_V2(seeds,poly_roots,c_calculation_depth,digits)
//' @param seeds complex vector
//' @param poly_roots complex roots vector # roots of the polynom => solve(poly)
//' @param c_calculation_depth #how many iterrations steps (depth of calculation)
//' @param digits how many decimal digit till calculation stop
//' @examples polynom <-polynom::polynomial(coef = c(-0.2,-1,1,0,0,1))
//' @examples roots <- solve(polynom)
//' @examples seeds <- expand.grid(seq(-2,2, length.out = 200),seq(-2,2, length.out = 200))
//' @examples seeds <- complex(real = seeds$Var1, imaginary = seeds$Var2)
//' @examples calc_it_V2(seeds,roots,100,6)
//' @export
/* this fixes it */
// [[Rcpp::export]]
DataFrame calc_it_V2
(
    std::vector<std::complex<double>> seeds,
    const std::vector<std::complex<double>> poly_roots,
    const int c_calculation_depth,
    const int digits
)
{
  std::vector<std::complex<double>> x_n(c_calculation_depth);
  IntegerVector roots (seeds.size());

  for (long long unsigned int ii = 0; ii < seeds.size(); ii++)
  {
    x_n[0] = seeds[ii];
    for (int jj = 1; jj < c_calculation_depth; jj++)
    {
      x_n[jj] = x_n[jj - 1] - (f(x_n[jj - 1]) / f_ableitung(x_n[jj - 1]));
    }
    roots[ii] = find_roots(x_n, poly_roots, digits);
  }
  // When giving names to columns
  DataFrame df = DataFrame::create( Named("seeds") = seeds , _["roots"] = roots );
  return(df);
}




//////////////////////////////////////////////
//'
//' @name calc_it_V3
//' @title calc_it_V3
//' @description Berechntet die Funktionswerte eines Polynoms für gegebenen Vektor.
//' @usage calc_it_V3(seeds,poly_roots,c_calculation_depth,digits)
//' @param seeds complex vector
//' @param poly_roots complex roots vector # roots of the polynom => solve(poly)
//' @param c_calculation_depth #how many iterrations steps (depth of calculation)
//' @param digits how many decimal digit till calculation stop
//' @examples polynom <-polynom::polynomial(coef = c(-0.2,-1,1,0,0,1))
//' @examples roots <- solve(polynom)
//' @examples seeds <- expand.grid(seq(-2,2, length.out = 200),seq(-2,2, length.out = 200))
//' @examples seeds <- complex(real = seeds$Var1, imaginary = seeds$Var2)
//' @examples calc_it_V3(seeds, roots, 100,6)
//' @export
/* this fixes it */
// [[Rcpp::export]]
DataFrame calc_it_V3
(
   const std::vector<std::complex<double>> seeds,
   const std::vector<std::complex<double>> poly_roots,
   const int c_calculation_depth,
   const int digits
)
{
  std::vector<std::complex<double>> x_n(c_calculation_depth);
  std::vector<int> roots(seeds.size());
  double distance = 0;
  int poly_size = (int)poly_roots.size();
  int seed_size = (int)seeds.size();
  int ii = 0;
  int jj = 0;

  for (ii = 0; ii < seed_size; ii++)
  {
   x_n[0] = seeds[ii];
   for (jj = 1; jj < c_calculation_depth; jj++)
   {
     x_n[jj] = x_n[jj - 1] - (f(x_n[jj - 1]) / f_ableitung(x_n[jj - 1]));

     for (int kk = 0; kk < poly_size; kk++)
     {
       distance = (double)abs(x_n[jj] - poly_roots[kk]);
       if (distance < pow(10, -digits)) {
         roots[ii] = kk + 1;
         jj = c_calculation_depth;
         break;
       }
     }
   }
  }
  // When giving names to columns
  DataFrame df = DataFrame::create( Named("seeds") = seeds , _["roots"] = roots );
  return(df);
}



 // You can include R code blocks in C++ files processed with sourceCpp
 // (useful for testing and development). The R code will be automatically
 // run after the compilation.
 //

/*** R
library(slvwagner)
#######
# Initale Funktion
#######
polynom <-polynom::polynomial(coef = c(-0.2,-1,1,0,0,1))
roots <- solve(polynom)
names(roots) <- as.character(1:length(roots))
f_R <- function(x) -0.2 - x + x^2 + x^5
f_ableitung_R <- function(x) -1 + 2*x + 5*x^4

#######
# seeds
c_dpi <- 20
seeds <- expand.grid(seq(-2,2, length.out = c_dpi),
                    seq(-2,2, length.out = c_dpi))
seeds <- complex(real = seeds$Var1, imaginary = seeds$Var2)

tibble::tibble("sapply_ploynom" = seeds|>sapply(function_calc_complex, polynom),
              "C_polynom" = seeds|>vector_function_calc_complex(polynom),
              f_R(seeds),
              f_vector(seeds),
              f_ableitung_R(seeds),
              f_ableitung_vector(seeds)
)

result<-microbenchmark::microbenchmark("sapply_ploynom" = seeds|>sapply(function_calc_complex, polynom),
                                      "C_polynom" = seeds|>vector_function_calc_complex(polynom),
                                      f_R(seeds),
                                      f_ableitung_R(seeds),
                                      f_vector(seeds),
                                      f_ableitung_vector(seeds)
)
result
result<-microbenchmark::microbenchmark(f_R(seeds),
                                      f_ableitung_R(seeds),
                                      f_vector(seeds),
                                      f_ableitung_vector(seeds)
)
result
plot(result)

#######
# roots
#######
find_root_R <- function(x_n,roots, digits){
 for(i in 1:length(roots)){
   temp <- x_n|>round(digits) ==
     round(roots[i],digits)
   if(sum(temp) > 0) {
     break
   }
 }
 return(i)
}

find_root_R_v2 <- function(x_n,roots, digits){
 deviation <- 10^digits
 for(i in 1:length(roots))
 {
   distance <- Mod(x_n-roots[i])
   if(sum(distance == digits)) {
     break
   }
 }
 return(i)
}

get_x_n <- function(seeds){
 x_n <- list()
 x_n[[1]] <- seeds-(f_R(seeds)/f_ableitung_R(seeds))
 for (i  in 2:length(seeds)) {
   x_n[[i]] <- x_n[[i-1]] -(f_R(x_n[[i-1]])/f_ableitung_R(x_n[[i-1]]))
 }
 return(x_n)
}
l_x_n <- get_x_n(seeds)

digits <- 6
result <- microbenchmark::microbenchmark(
 "find roots R" = find_root_R(l_x_n[[1]],roots,digits),
 "find roots R V2" =find_root_R_v2(l_x_n[[1]],roots,digits),
 "find roots C" = find_roots(l_x_n[[1]],roots,digits),
 "find roots C V2"=find_roots_v2(l_x_n[[1]],roots,digits)
)
result
plot(result)

#######
calc_it_R <- function(x,roots,depth,digits){
 x|>
   lapply(function(x){
     x_n <- 1:depth
     x_n[1] <- x
     for( i in 2:depth) {
       x_n[i] <- x_n[i-1]-(f_R(x_n[i-1])/f_ableitung_R(x_n[i-1]))
     }
     return(data.frame(seed = x,
                       root = find_root_R(x_n, roots, digits)))

   })|>
   dplyr::bind_rows()
}

seeds|>
 sapply(function(x){
   x_n <- 1:100
   x_n[1] <- x
   for( i in 2:length(x_n)) {
     x_n[i] <- x_n[i-1]-(function_calc_complex(x_n[i-1],polynom)/function_calc_complex(x_n[i-1], deriv(polynom)))
   }
   return(find_roots(x_n,roots,3))
 })
seeds|>
 sapply(function(x){
   x_n <- 1:100
   x_n[1] <- x
   for( i in 2:length(x_n)) {
     x_n[i] <- x_n[i-1]-(f_R(x_n[i-1])/f_ableitung_R(x_n[i-1]))
   }
   return(find_root_R(x_n,roots,3))
 })

seeds|>calc_it_R(roots,100,digits)==seeds|>calc_it(polynom,deriv(polynom),roots,100,digits)

seeds[c(1,8,119,122,136,353)]|>calc_it_R(solve(polynom),100,digits)
seeds[c(1,8,119,122,136,353)]|>calc_it(polynom,deriv(polynom),solve(polynom),100,digits)
seeds[c(1,8,119,122,136,353)]|>calc_it_V2(solve(polynom),100,digits)
seeds[c(1,8,119,122,136,353)]|>calc_it_V3(solve(polynom),100,digits)


result <- microbenchmark::microbenchmark(
 lapply_R =
   #######
 # serial calculation for web
 data.frame(seed = seeds,
            root = seeds|>
              sapply(function(x){
                x_n <- 1:100
                x_n[1] <- x
                for( i in 2:length(x_n)) {
                  x_n[i] <- x_n[i-1]-(f_R(x_n[i-1])/f_ableitung_R(x_n[i-1]))
                }
                return(find_root_R(x_n,roots,3))
              })
 ),
 lapply_C =
   #######
 # serial calculation for web
 data.frame(seed = seeds,
            root =  seeds|>
              sapply(function(x){
                x_n <- 1:100
                x_n[1] <- x
                for( i in 2:length(x_n)) {
                  x_n[i] <- x_n[i-1]-(f_R(x_n[i-1])/f_ableitung_R(x_n[i-1]))
                }
                return(find_roots(x_n,roots,3))
              })
 ),
 "calc_it R" = seeds|>calc_it_R(roots,100,digits),
 "calc_it C" = seeds|>calc_it(polynom,deriv(polynom),roots,100,digits),
 "calc_it_V2 C" = seeds|>calc_it_V2(roots,100,digits),
 "calc_it_V3 C" = seeds|>calc_it_V3(roots,100,digits),
 times = 10
)
result
plot(result)

#######
# seeds
c_dpi <- 200
seeds <- expand.grid(seq(-2,2, length.out = c_dpi),
                    seq(-2,2, length.out = c_dpi))
seeds <- complex(real = seeds$Var1, imaginary = seeds$Var2)

digits <- 6
result <- microbenchmark::microbenchmark(
 "calc_it_V2 C" = seeds|>calc_it_V2(roots,100,digits),
 "calc_it_V3 C" = seeds|>calc_it_V3(roots,100,digits),
 times = 10
)
result
plot(result)

*/
