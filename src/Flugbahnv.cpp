#include <Rcpp.h>
#include <Rmath.h>
#include <vector>

using namespace Rcpp;

// //Konstanten
const static double g = -9.81;        // [m / s ^ 2]

// Betrag eines vectors
double long betrag( double x, double y) {
  return  sqrt(R_pow( x,  2) +  R_pow( y, 2));
}

void rcpp_rcout(NumericVector v){
  // printing value of vector
  Rcout << "Values of Vector: " << v << "\n";
}

void rcpp_error_out(CharacterVector v){
  // printing value of vector
  Rcerr <<  v << "\n";
}

///////////////////////////////////////////////
//' Flugbahn
//'
//' @name Flugbahn
//' @description Berechnet die Flugbahn eines Geschosses mit berücksichtigung des Luftwiderstandes.
//' Es wird über den Zeitintervall "t" dv, und dx,dy berechnet.
//' @usage Flugbahn(v0,t,angle, target_vector, m, k,nb_return_values)
//' @param v0 Geschosss-Geschwindigkeit Vektor
//' @param t Zeit (interval der Berechnung)
//' @param angle Schusswinkel
//' @param target_vector target_vectorvektor
//' @param m Masse des Geschosses
//' @param k k-Wert = 0.5*cw*rho*A: cw ==> Strömungswiderstandskoeffizient, Widerstandsbeiwert, rho ==> Dichte (kg/m^3), A ==> Fläche (m^2);
//' @param nb_return_values Anzahl Werte die zurückgegeben werden. Wenn Null gewählt wird dann werden alle brechneten Werte zurückgegenen.
//' @return matrix
//' @examples # target_vector
//' @examples target_vector <- c(100, 20) # m
//' @examples angle <- atan(target_vector[2]/target_vector[1]) #rad
//' @examples # Zeitintervall
//' @examples t <- 1e-5
//' @examples # Geschoss
//' @examples v0 = 55 # [m/s]
//' @examples m <- 0.440 # [kg]
//' @examples cw <- 0.18; # cw Wert des Geschosses (Nach Tabellen der Geschossform)
//' @examples rho <- 1.293 # Dichte des Mediums (Luft) [kg/m^3]
//' @examples A <- (0.22)^2*pi/4 #Projektions Flaeche in Flugrichtung [m^2]
//' @examples k <- 0.5*cw*rho*A
//' @examples Flugbahn_c <- Flugbahn(v0,t,angle,target_vector,m,k,1000)[,4:5]
//' @examples colnames(Flugbahn_c) <- c("sx","sy")
//' @examples Flugbahn_c|>
//' @examples   plot(type = "l")
//' @examples #tidyvers
//' @examples library(tidyverse)
//' @examples Flugbahn_c|>
//' @examples   as_tibble()|>
//' @examples   ggplot(aes(sx,sy))+
//' @examples   geom_line()+
//' @examples   geom_point(data = tibble(x = target_vector[1],y = target_vector[2]),aes(x,y), color = "red")+
//' @examples   geom_text(aes(x = target_vector[1],y = target_vector[2]*1.1, label = "target_vector"))+
//' @examples   coord_fixed()+
//' @examples   labs(title = "Flugbahn Rcpp", subtitle = paste("v0:",v0,"\nt:",t))
//' @return matrix t,v_x, v_y, s_x, s_y, distance (to target)
//' @export
/* this fixes it */
// [[Rcpp::export]]
NumericMatrix Flugbahn
  (const double v0,
   const double t ,
   const double angle,
   const NumericVector target_vector,
   const double m,
   const double k,
   const int nb_return_values)
{
  //Beschleunigung
  double  a = (-1) * (R_pow(v0, 2) * k) / m;

    //Geschwindigkeitsänderung
  double  dv_x = a * cos(angle) * t;
  double  dv_y = (a * sin(angle) + g) * t; // -Erdbeschleunigung -9.81

  // Number of columns of Matrix
  const static int column = 6;
  //// Speichern der Werte in Vektoren (dynamische Länge)

  // v_x
  std::vector<double> v_x(1,(v0*cos(angle)));
  //NumericVector v_x = {(v0*cos(angle))};
  v_x.push_back(v_x[0] + dv_x);
  //Rcout << "v_x: " << v_x[0] << ", " << v_x[1] <<"\n";

  // v_y
  std::vector<double> v_y(1, (v0*sin(angle)));
  //NumericVector v_y = {v0*sin(angle)};
  v_y.push_back(v_y[0] + dv_y);
  //Rcout << "v_y: " << v_y[0] << ", " << v_y[1] << "\n";

  // s_x
  std::vector<double> s_x(1, 0);
  //NumericVector s_x ={0};
  s_x.push_back(v_x[0]*t);
  //Rcout << "s_x: " << s_x[0] << ", " << s_x[1] << "\n";

  // s_y
  std::vector<double> s_y(1, 0);
  //NumericVector s_y ={0};
  s_y.push_back(v_y[0]*t);
  //Rcout << "s_y: " << s_y[0] << ", " << s_y[1] << "\n";

  // time
  std::vector<double> vec_t(1, 0);
  //NumericVector vec_t ={0};
  vec_t.push_back(t);
  //Rcout << "t:   " << vec_t[0] << ", " << vec_t[1] << "\n";

  // distance
  double dist_ = betrag(target_vector[0], target_vector[1]);
  std::vector<double> vec_dist(1, dist_);
  //NumericVector  vec_dist = {dist_};
  vec_dist.push_back(betrag(target_vector[0] - s_x[1], target_vector[1] - s_y[1]));
  //Rcout << "Distance: " << vec_dist[0] << ", "<< vec_dist[1] << "\n***************************************************\n";


  //////////////////////////////////////////////////////////////////////////////////////
  // Calculation
  // letzter Flugwinkel
  long double angle_ ;

  // letzte Geschwindigkeit
  long double v0_ ;

  // Flugbahn
  int run = 1;
  unsigned long int i = 2;
  while(run == 1){
    // Flugwinkel
    angle_ = atan((s_y[i-1] - s_y[i-2]) / (s_x[i-1] - s_x[i-2]));
    // letzte Distanz
    dist_ = betrag((target_vector)[0] - s_x[i-1], (target_vector)[1] - s_y[i-1]);
    // letzte Geschwindigkeit
    v0_ = betrag(v_x[i - 1], v_y[i - 1]);
    // Luftwiderstand (Beschleunigung entgegen der Flugrichtung)
    a = -(1) * (pow(v0_, 2) * (k)) / (m);
    //Geschwindigkeitsänderung
    dv_x = a * cos(angle_) * t;
    dv_y = (a * sin(angle_) - 9.81) * t;   // -Erdbeschleunigung -9.81

    // Neue Geschwindigkeit
    v_x.push_back(v_x[i - 1] + dv_x); // alte Geschwindigkeit x + delta dv_x
    v_y.push_back(v_y[i - 1] + dv_y); // alte Geschwindigkeit y + delta dv_y
    // Neue Position
    s_x.push_back(s_x[i - 1] + v_x[i] * t); // alte Position + v_x*t
    s_y.push_back(s_y[i - 1] + v_y[i] * t); // alte Position + v_y*t

    //Abbruch wenn die alte Distanz kleiner als die neue
    if (dist_ < betrag(target_vector[0] - s_x[i], target_vector[1] - s_y[i]) || std::isnan(v_x[i])) {
      run = 0;
      //Zeit
      vec_t.push_back(t * i);
      //distanz
      vec_dist.push_back(dist_);
    }
    //Zeit
    vec_t.push_back(t * i);
    //distanz
    vec_dist.push_back(dist_);

    i += 1;
  }


  //////////////////////////////////////////////////////////////////////////////////////
  // Wieviele Werte sollen an R zurückgegeben werden
  int vec_size = nb_return_values; // specified rows
  if(nb_return_values == 0) // if zero is defined use all rows
  {
    vec_size = v_x.size();
  }
  Rcpp::NumericMatrix result(vec_size,column);
  colnames(result) = CharacterVector({"t","v_x","v_y","s_x","s_y","distance"});

  //Alle Werte zurückgeben
  if(nb_return_values == 0)
  {
    //Abfüllen aller Wert
    for(int i = 0; i < vec_size; i++)
    {
      result(i,0) = vec_t[i];     // time
      result(i,1) = v_x[i];       // v_x velocity x
      result(i,2) = v_y[i];       // v_y velocity y
      result(i,3) = s_x[i];       // s_x position x
      result(i,4) = s_y[i];       // s_y position y
      result(i,5) = vec_dist[i];  // distance to target
    }
  }
  else // Nur die vorgegebenen Anzahl Werte zurückgeben
  {
    //Packet grösse
    int ratio = int(vec_t.size() / nb_return_values);
    // Rcout << "vec_size: " << vec_size << "\n";
    // Rcout << "ratio double: " << ratio << "\n*************\n";
    // Rcout << "ratio int: " << int(ratio) << "\n*************\n";

    //Abfüllen der Werte
    for(int i = 0; i < vec_size; i++) {
      result(i,0) = vec_t[(i*ratio)];     // time
      result(i,1) = v_x[(i*ratio)];       // v_x velocity x
      result(i,2) = v_y[(i*ratio)];       // v_y velocity y
      result(i,3) = s_x[(i*ratio)];       // s_x position x
      result(i,4) = s_y[(i*ratio)];       // s_y position y
      result(i,5) = vec_dist[(i*ratio)];  // distance to target
    }
  }
  return result;
}


/*** R
library(slvwagner)

# target_vector
target_vector <- c(100, 20) # m

angle <- atan(target_vector[2]/target_vector[1]) #rad
# Zeitintervall
t <- 1e-5
# Geschoss
v0 = 55 # [m/s]
m <- 0.440 # [kg]
cw <- 0.18; # cw Wert des Geschosses (Nach Tabellen der Geschossform)
rho <- 1.293 # Dichte des Mediums (Luft) [kg/m^3]
A <- (0.22)^2*pi/4 #Projektions Flaeche in Flugrichtung [m^2]
k <- 0.5*cw*rho*A

Flugbahn(v0,t,angle,target_vector,m,k,1000)|>head()
Flugbahn(v0,t,angle,target_vector,m,k,1000)|>tail()

Flugbahn(v0,t,angle,target_vector,m,k,0)|>head()
Flugbahn(v0,t,angle,target_vector,m,k,0)|>tail()

Flugbahn_c <- Flugbahn(v0,t,angle,target_vector,m,k,0)[,c("s_x","s_y")]
Flugbahn_c|>
  tibble::as_tibble()|>
  plot(type = "l", main ="all data")

Flugbahn_c <- Flugbahn(v0,t,angle,target_vector,m,k,1000)[,c("s_x","s_y")]
Flugbahn_c|>
  tibble::as_tibble()|>
  plot(type = "l", main = "1000 rows")

library(tidyverse)
Flugbahn_c|>
  as_tibble()|>
  ggplot(aes(s_x,s_y))+
  geom_line()+
  geom_point(data = tibble(x = target_vector[1],y = target_vector[2]),aes(x,y), color = "red")+
  geom_text(data = tibble(x = target_vector[1],y = target_vector[2]),
            aes(x ,y, label = "target vector"),
            nudge_y = 2.5)+
  coord_fixed()+
  labs(title = "Flugbahn Rcpp", subtitle = paste("v0:",v0,"\nt:",t,"s"))

results <- microbenchmark::microbenchmark(#RcExtentions = RCExtension::Flugbahn(v0,t,angle,target_vector,m,k),
                                          Rcpp_all = Flugbahn(v0,t,angle,target_vector,m,k,0),
                                          Rcpp_n = Flugbahn(v0,t,angle,target_vector,m,k,100),
                                          times = 10)
results
plot(results)


*/
