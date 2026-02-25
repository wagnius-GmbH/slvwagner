#include <Rcpp.h>
#include <vector>
#include <cmath>      // for std::abs
#include <Rmath.h>

using namespace Rcpp;

//Konstanten
const static double g = -9.81;        // [m / s ^ 2]
const static long double pi = 3.141592653589793238462643383279502884197169399375; //Kreiszahl

/// Definition of ODE for Penulum calculation
double get_theta_double_dot(double theta, double theta_dot, double L, double mu)
{
  return -mu*theta_dot - (g/L)*sin(theta);
}


///////////////////////////////////////////////
//' pendulum_motion
//'
//' @name pendulum_motion
//' @description Motion of pendulum.
//' @param L Länge des Pendels (m)
//' @param delta_t (s)
//' @param THETA_0 Startwinkel (rad)
//' @param THETA_DOT_0 Anfangsgeschwindigkeit (m/s^2)
//' @param mu Reibungkoeffizient
//' @param calculation_stop Abbruch wenn die Geschwindigkeit kleiner und der Winkel kleiner als calculation_stop
//' @param nb_return_values Anzahl Werte die zurückgegeben werden. Wenn Null gewählt wird dann werden alle brechneten Werte zurückgegenen.
//' @return matrix
//' @examples t = 35 ;
//' @examples L = 2; delta_t = 1e-5; THETA_0 = pi/3; THETA_DOT_0 = 3; mu = 0.1;
//' @examples calculation_stop = 0.05; nb_return_values = 800 ;
//' @examples pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values)|>
//' @examples plot(type = "l", main = "RccpTest: Pendulum Motion",xlab = "t", ylab = "angle")
//' @examples pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values)[,c(1,3)]|>
//' @examples plot(type = "l", main = "RccpTest: Pendulum Motion",xlab = "t", ylab = "angular velosity")
//' @export
/* this fixes it */
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
NumericMatrix pendulum_motion(const int L,
                               const double delta_t,
                               const double THETA_0, const double THETA_DOT_0,
                               const double mu,
                               const double calculation_stop,
                               const int nb_return_values)
 {
   // Werte die abgespeichert werden und an R zurueckgegeben werden
   const int column = 3;             // colums of matrix
   double theta = (THETA_0);         // angel
   double theta_dot = (THETA_DOT_0); // angular velocity
   double theta_double_dot;          // angular acceleration

   // variable length vectors
   std::vector<double> p_theta(1, theta);          // angel (Position of Pendulum)
   std::vector<double> p_theta_dot(1, theta_dot);  // angular velocity

   // FIXED: Changed abs() to std::abs()
   int stop_1 = std::abs(theta_dot) > calculation_stop;
   int stop_2 = std::abs(std::fmod(theta, -pi / 2)) > calculation_stop;

   while (stop_1 || stop_2)
   {
     theta_double_dot = get_theta_double_dot(theta, theta_dot, (L), (mu)); // angular acceleration
     theta = theta + (theta_dot * (delta_t));
     theta_dot = theta_dot + (theta_double_dot * (delta_t));
     p_theta.push_back(theta);
     p_theta_dot.push_back(theta_dot);

     // FIXED: Changed abs() to std::abs()
     stop_1 = std::abs(theta_dot) > calculation_stop;
     stop_2 = std::abs(std::fmod(theta, -pi / 2)) > calculation_stop;
   }

   // Wieviele Werte sollen an R zurückgegeben werden
   //Anzahl Werte
   int vec_size = nb_return_values;
   if (nb_return_values == 0)
   {
     vec_size = p_theta.size();
   }

   //Matrix an R
   NumericMatrix result(vec_size, column);
   colnames(result) = CharacterVector({"time", "angle", "angular velocity"});

   //Alle Werte zurückgeben
   if (nb_return_values == 0)
   {
     result(0, 0) = 0;                  // time
     result(0, 1) = p_theta[0];         // angel (pendulum position)
     result(0, 2) = p_theta_dot[0];     // angular velocity

     //Abfüllen aller Wert
     for (long int i = 1; i < vec_size; i++)
     {
       result(i, 0) = delta_t * i;          // time
       result(i, 1) = p_theta[i];         // angel (pendulum position)
       result(i, 2) = p_theta_dot[i];     // angular velocity
     }
   }
   else // Nur die vorgegebenen Anzahl Werte zurückgeben
   {
     //Packet grösse
     int ratio = int(p_theta.size() / nb_return_values);
     //Rcout << "\nvec_size: " << vec_size << "\n************************\n";

     //Abfüllen der Werte
     for (long int i = 0; i < nb_return_values; i++)
     {
       result(i, 0) = delta_t * i * ratio;    // time
       result(i, 1) = p_theta[i * ratio];     // angel (pendulum position)
       result(i, 2) = p_theta_dot[i * ratio]; // angular velocity
     }
   }
   return result;
 }

/*** R
library(slvwagner)
t = 35 ;
L = 2; delta_t = 1e-5; THETA_0 = pi/3; THETA_DOT_0 = 3; mu = 0.1;
calculation_stop = 0.05; nb_return_values = 1000;

pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values = 0)|>
  head()
pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values = 0)|>
  tail()
pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values = 100)|>head()
pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values = 100)|>tail()

pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values)|>
  plot(type = "l", main = "RccpTest: Pendulum Motion",xlab = "t", ylab = "angle")

results <- microbenchmark::microbenchmark(
  RcppTest_all = pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values = 0),
  RcppTest_n   = pendulum_motion(L, delta_t, THETA_0, THETA_DOT_0, mu, calculation_stop, nb_return_values = nb_return_values),
  times = 3)
results
plot(results)
*/
