#######################################
#' Intersection point(s) of line and sphere.
#'
#' @name geo_interSec_sph_line
#' @details Computes the intersecting point of a line and the surface of a spear.
#' @param p position vector for the line  c(x = ,y = ,z = )
#' @param u direction vector for the line c(x = ,y = ,z = )
#' @param m position vector of the centre of the sphere c(x = ,y = ,z = )
#' @param r radius of the sphere
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' list of intersecting point(s)
#' @examples
#' p <- c(x = 1, y = -2, z = 3)
#' u <- c(x = 1, y = 0, z = 1)
#' m <- c(1,-2,4)
#' r <- 1
#' geo_interSec_sph_line(p, u, m, r)
#' @export

geo_interSec_sph_line <- function(p, u, m, r) {
  ## Formula
  t_expression <-expression(c((sqrt(((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+u[3]*
                                       (p[3]-m[3]))^2-4*(u[1]^2+u[2]^2+u[3]^2)*((p[1]-m[1])^2+(p[2]-m[2])^2+(p[3]-m[3])^2-r^2))-
                                 ((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+
                                    u[3] * (p[3] - m[3])))/(2 * (u[1]^2 + u[2]^2 + u[3]^2)),
                              (-((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+u[3]*(p[3]-m[3])+
                                   sqrt(((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+u[3]*
                                           (p[3]-m[3]))^2-4*(u[1]^2+u[2]^2+u[3]^2)*((p[1]-m[1])^2+(p[2]-m[2])^2+(p[3]-m[3])^2-r^2))))/
                                (2*(u[1]^2 + u[2]^2 + u[3]^2))))

  # Parameter t
  c_result <- eval(t_expression, list(p, u, m, r))
  names(c_result) <- rep("",length(c_result))

  # Intersection points
  l_Result <- list()
  for (ii in 1:length(c_result)) {
    l_Result[[ii]] <- c(p + (c_result[ii] * u))
  }

  if(is.na(c_result)|>sum() == 0){
    return(l_Result)
  }else{
    print(warning())
    return(NULL)
  }
}

#######################################
#' Conic section from five point
#'
#' @name geo_conic_section_from_5points
#' @description calculate conic section from five given points \code{section_points}
#' @details \eqn{Ax^2+Bxy+Cy^2+Dx+Ey+F=0}
#' @details <https://en.wikipedia.org/wiki/Conic_section>
#' @details The algorithm determines fist the type of conic section. If the conic section results in an ellipse or a circle the function returns useful values
#' such as the equation and the centre of the ellipse or circle.
#' Additionally you can define the number of points returned \code{nb} to visualize the ellipse or the circle.
#' For an ellipse additionally the semi \eqn{b} and major axis \eqn{a} of the ellipse are provided.
#' @param section_points matrix of section points, one point per row.
#' @param nb number of points returned to plot the ellipse
#' @author Florian Wagner \email{florian.wagner@wagnius.ch}
#' @returns
#' list, check out the example.
#'
#' @examples
#' section_points <- matrix(c(-1,3,
#'                           2,4,
#'                           6,2,
#'                           8,-5,
#'                           1,-3),
#'                           nrow = 5, byrow = TRUE)
#' l_list <- geo_conic_section_from_5points(section_points, nb = 20)
#' l_list
#' l_list$df|>plot(asp = 1)
#' abline(a = l_list$lf_a_b[1,2], b = l_list$lf_a_b[1,1])
#' abline(a = l_list$lf_a_b[2,2], b = l_list$lf_a_b[2,1])
#' points(x = l_list$center[1],y = l_list$center[2])
#'
#' # hyperbola
#' matrix(c(-2,1,
#'          -1,3,
#'          1,-1.5,
#'          0.8,0.8,
#'          2,2.8),
#'          nrow = 5, byrow = TRUE)|>
#'    geo_conic_section_from_5points()
#'
#' #circle
#' l_list <- (matrix(c(-1,0,
#'          0,1,
#'          1,0,
#'          0,-1,
#'          cos(pi/4),sin(pi/4)),
#'          nrow = 5, byrow = TRUE)+0.2)|>
#'    geo_conic_section_from_5points(nb = 20)
#' l_list
#' l_list$df|>plot(asp=1)
#'
#' #parabola
#' matrix(c(-2,3,
#'          -1,0,
#'          0,-1,
#'          1,0,
#'          2,3),
#'          nrow = 5, byrow = TRUE)|>
#'    geo_conic_section_from_5points()
#' @export

geo_conic_section_from_5points <- function(section_points, nb = 10){
  A <- matrix(c(section_points[1,1]^2,section_points[1,1]*section_points[1,2],section_points[1,2]^2,section_points[1,1],section_points[1,2],
                section_points[2,1]^2,section_points[2,1]*section_points[2,2],section_points[2,2]^2,section_points[2,1],section_points[2,2],
                section_points[3,1]^2,section_points[3,1]*section_points[3,2],section_points[3,2]^2,section_points[3,1],section_points[3,2],
                section_points[4,1]^2,section_points[4,1]*section_points[4,2],section_points[4,2]^2,section_points[4,1],section_points[4,2],
                section_points[5,1]^2,section_points[5,1]*section_points[5,2],section_points[5,2]^2,section_points[5,1],section_points[5,2]),
              nrow = 5, byrow = T)

  b <- matrix(rep(-1,5))

  P <- solve(A,b)
  P <- rbind(P,1)
  rownames(P)<- LETTERS[1:6]

  M0 <- matrix(c(P["F",]  , P["D",]/2, P["E",]/2,
                 P["D",]/2, P["A",]  , P["B",]/2,
                 P["E",]/2, P["B",]/2, P["C",]),
               nrow=3, byrow=TRUE)

  M <- matrix(c(P["A",],P["B",]/2,
                P["B",]/2,P["C",]),nrow = 2 ,byrow = T)

  lambda <- eigen(M)$values
  if((lambda[1]-P["A",])<=(lambda[2]-P["C",])){
    lambda <- c(lambda[2],lambda[1])
  }
  M_eigen <- eigen(M)

  if(det(M0) != 0){
    if(det(M)<0) {
      #writeLines("hyperbola")
      return(list(type = "hyperbola",
                  EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                  "section points" = section_points))
    }
    else if (det(M)==0) {
      #writeLines("parabola")
      return(list(type = "parabola",
                  EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                  "section points" = section_points))
    }
    else if (det(M)>0) { #either ellipse or circle

      a <- sqrt(-det(M0)/(det(M)*lambda[1]))
      b <- sqrt(-det(M0)/(det(M)*lambda[2]))

      center <- solve(M,c(-P["D",]/2,-P["E",]/2))|>as.vector()
      t <- seq(0, 2*pi, len = nb)

      if(a==b){
        #writeLines("circle")
        df <- data.frame(x = (a*cos(t)) + center[1],
                         y = (a*sin(t)) + center[2]
        )
        return(list(type = "circle",
                    EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                    "x(t)" = paste0(a,"*cos(t) +",center[1]),
                    "y(t)" = paste0(a,"*sin(t) +",center[2]),
                    df = df,
                    center = center,
                    "Input: Section points" = section_points)
        )
      }else{
        #writeLines("ellipse")
        m <- M_eigen$vectors[2,]/M_eigen$vectors[1,]
        lf <- matrix(c(m, center[2]-m*center[1]),nrow = 2, byrow = F)|>
          as.data.frame()|>
          setNames(c("slope","intercept"))

        phi <- ifelse((lambda[1]-P["A",])<=(lambda[2]-P["C",]),
                      atan(m[2]),
                      atan(m[1]))
        names(phi)<-NULL

        df <- data.frame(x = center[1] + a*cos(t)*cos(phi) - b*sin(t)*sin(phi),
                         y = center[2] + a*cos(t)*sin(phi) + b*sin(t)*cos(phi)
        )

        return(list(type = "ellipse",
                    EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                    "x(t)" = paste0(center[1],"+", a, "*cos(t)*cos(",phi,") - ",b,"*sin(t)*sin(",phi,")"),
                    "y(y)" = paste0(center[2],"+ ",a, "*cos(t)*sin(",phi,") + ",b,"*sin(t)*cos(",phi,")"),
                    df = df,
                    center = center,
                    a = a,
                    b = b,
                    phi = phi,
                    lf_a_b = lf,
                    "Input: Section points" = section_points)
        )
      }
    }
    else if (A == C & B == 0) {
      #writeLines("circle")
      df <- data.frame(x = (a*cos(t)) + center[1],
                       y = (a*sin(t)) + center[2]
      )
      return(list(type = "circle",
                  EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                  "x(t)" = paste0(a,"*cos(t) +",center[1]),
                  "y(t)" = paste0(a,"*sin(t) +",center[2]),
                  df = df,
                  center = center,
                  "Input: Section points" = section_points)
      )
    }
  }else {
    return(list(type = "degenerate",
                "Input: Section points" = section_points)
    )
  }
}

#######################################
#' slerp  by 3 points and a given radius.
#'
#' @name geo_slerp
#' @description
#' Radius interpolation by 3 points and a given radius.
#' The Radius interpolation will be calculated using the \code{cp} = common center point and the vector \code{x1} and \code{x2}.
#' @details
#' \deqn{sin((1 - t) * phi) / sin(phi) * x1   +  sin(t * phi / sin(phi)  *  x2)}
#' \url{https://en.wikipedia.org/wiki/Slerp}
#' @param  R radius
#' @param  x1 vector
#' @param  x2 vector
#' @param  cp vector
#' @param  nb_points count of points to be generate by the function.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Matrix with \code{nrow(nb_points)}
#' @examples
#' p <- t(as.matrix(data.frame(x1 = c(-10,-5,50),
#'                             x2 = c(-20,10,2),
#'                             cp = c(10,-10,10))))
#'
#' m <- geo_slerp(R  =10,
#'                 x1 = p["x1",],
#'                 x2 = p["x2",],
#'                 cp = p["cp",],
#'                 nb_points = 20)
#' m <- rbind(p,m)
#' #Plot 3D
#' library(rgl)
#' plot3d( m[,1], m[,2], m[,3], type = "p", lwd = 2, top = TRUE,
#'         #col = rainbow(nrow(m)),
#'         aspect = c(diff(c(min(m[,1]),max(m[,1]))),
#'                    diff(c(min(m[,2]),max(m[,2]))),
#'                    diff(c(min(m[,3]),max(m[,3])))
#'                    ))
#' @export

geo_slerp <- function(R,x1,x2,cp,nb_points = 10) { #slerp aus drei Punkten, Radius, Punkteanzahl
  #(cp => Ortsvektor)
  #Verschieben des Koordinatensystems: Neuer Ursprung cp
  sp <- x1-cp #Stützvektor
  ep <- x2-cp #Stützvektor

  #Stuetzvektoren
  s1l   <- sp-c(0,0,0) #Stützvektor aus Ortsvektor und Startpunkt
  s2l   <- ep-c(0,0,0) #Stützvektor aus Ortsvektor und Endpunkt
  #Stuetzvektoren gleich gross machen
  s1 <- s1l*math_betrag(s2l)
  s2 <- s2l*math_betrag(s1l)
  #Skallierung der Stuetzvektoren auf Betrag = 1 => Einheitsvektoren
  skalierung <- math_betrag(s1)
  s1 <- s1/skalierung
  s2 <- s2/skalierung
  #Zwischenwinkel der beiden Vektoren
  phi<-math_inbetweenAngle(s1,s2)
  #Slerp zwischen den Einheitsvektoren mit s1,s2
  t <- seq(0,1,1/nb_points)#Zahl zwischen Null und eins 0...1 die den Winkel aufteilt
  #slerp Berechnung
  slerp <- t(sapply(sin((1-t)*phi)/sin(phi), function(i) i*s1, simplify = TRUE)+
               sapply(sin(t*phi)/sin(phi), function(i) i*s2, simplify = TRUE ))
  #Vektoren strecken mit vorgegebenen Radius R
  slerp <- slerp*R
  #Koordinaten um cp (Ortsvektor) verschieben
  return(t(apply(slerp, 1, function(i) i + cp)))
}


#######################################
#' Convert plane in coordinate form to parameter from
#'
#' @name geo_convert_plane_coord_to_param
#' @description
#' Get parametric function for a plane with the parameter s,t: \deqn{\vec x = \vec p + s \cdot \vec u + t \cdot \vec v}
#' The function returns a position vector \deqn{\vec p}, and two directions vectors \deqn{\vec u, \vec v}.
#' The Vector \deqn{\vec p} is also the normal vector (perpendicular to the plane).
#' @details
#' The coordinate form of the plane is:
#' \deqn{a x + b y + c z - d = 0}
#' where \deqn{d = \vec p \cdot \vec n} and \deqn{\vec n = \begin{pmatrix} a \\ b \\ c \end{pmatrix}}
#' \url{https://de.wikipedia.org/wiki/Ebenengleichung#Normalenform}
#' \url{https://en.wikipedia.org/wiki/Euclidean_planes_in_three-dimensional_space}
#' @param  E Plane equation (Ryacas)
#' @param  axis character vector indentifiying the axis variables  e.g. c("x1","x2","x3")
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' list with matrix containing the Vectors
#' \deqn{\vec p, \vec u, \vec v}
#' and \deqn{\vec n = \begin{pmatrix} a \\ b \\ c \end{pmatrix},-d}
#' @examples
#' geo_convert_plane_coord_to_param("10*x+15*y+20*z+50")
#' geo_convert_plane_coord_to_param("-10*x+15*y+20*z+50")
#' geo_convert_plane_coord_to_param("10*x-15*y+20*z+50")
#' geo_convert_plane_coord_to_param("10*x+15*y-20*z+50")
#' geo_convert_plane_coord_to_param("10*x+15*y+20*z-50")
#' geo_convert_plane_coord_to_param("-10*x-15*y+20*z+50")
#' geo_convert_plane_coord_to_param("10*x-15*y-20*z+50")
#' geo_convert_plane_coord_to_param("-10*x+15*y-20*z+50")
#' geo_convert_plane_coord_to_param("-10*x-15*y+20*z-50")
#' geo_convert_plane_coord_to_param("10*x-15*y-20*z-50")
#' geo_convert_plane_coord_to_param("-10*x+15*y-20*z-50")
#' geo_convert_plane_coord_to_param("10*x+15*y+50")
#' geo_convert_plane_coord_to_param("10*x+15*z+50")
#' geo_convert_plane_coord_to_param("10*y+15*z+50")
#' geo_convert_plane_coord_to_param("10*x+15*y-50")
#' geo_convert_plane_coord_to_param("10*x+15*z-50")
#' geo_convert_plane_coord_to_param("10*y+15*z-50")
#' geo_convert_plane_coord_to_param("-10*x+15*y+50")
#' geo_convert_plane_coord_to_param("-10*x+15*z+50")
#' geo_convert_plane_coord_to_param("-10*y+15*z+50")
#' geo_convert_plane_coord_to_param("10*x-15*y+50")
#' geo_convert_plane_coord_to_param("10*x-15*z+50")
#' geo_convert_plane_coord_to_param("10*y-15*z+50")
#' geo_convert_plane_coord_to_param("-10*z-50")
#' geo_convert_plane_coord_to_param("-10*y-50")
#' geo_convert_plane_coord_to_param("-10*x-50")
#' geo_convert_plane_coord_to_param("-10*z+50")
#' geo_convert_plane_coord_to_param("-10*y+50")
#' geo_convert_plane_coord_to_param("-10*x+50")
#' @export

geo_convert_plane_coord_to_param <- function(E,axis = c("x","y","z")) {
  # assign plane to yacas variable
  Ryacas::yac_assign(E, "e")
  # get parameters from plane equation
  x <- Ryacas::ysym(paste0("Coef(e,",axis[1],",1)"))
  y <- Ryacas::ysym(paste0("Coef(e,",axis[2],",1)"))
  z <- Ryacas::ysym(paste0("Coef(e,",axis[3],",1)"))
  # get normal vector
  n <- c(x,y,z)|>Ryacas::as_r()
  d <- -((E+paste0(-n[1],"*x+",-n[2],"*y+",-n[3],"*z")|>Ryacas::ysym())|>
           Ryacas::y_fn("Expand"))|>Ryacas::as_r()
  # Find intercept with the axis
  p <- d/n
  p <- ifelse(is.infinite(p),0,p)
  # Assign found axis intercept to vector
  p1 <- c(p[1],0,0)
  p2 <- c(0,p[2],0)
  p3 <- c(0,0,p[3])
  # Find orientation vectors
  u <- p1-p2
  u <- u/slvwagner::math_betrag(u)
  u <- ifelse(is.nan(u),0,u)
  v <- p2-p3
  v <- v/slvwagner::math_betrag(v)
  v <- ifelse(is.nan(v),0,v)
  # compute normal vector pointing to plane (Perpendicular to plane)
  p <- (slvwagner::math_betrag(d)/slvwagner::math_betrag(n))*n/slvwagner::math_betrag(n)
  if(d < 0) p <- -p
  # If vector u or v is zero it needs to be replaced by predefined vector or if the magnitude of u and v are equal
  if(sum(u)==0 | sum(v)==0 | (slvwagner::math_betrag(u)==slvwagner::math_betrag(v))){
    #print("u or v are zero, so using predefined vector")
    # plane is perpendicular to x
    if(p[2]==0 & p[3]==0){
      u <- c(0,1,0)
      v <- c(0,0,1)
      }
    # plane is perpendicular to y
    else if(p[1]==0 & p[3]==0){
      u <- c(1,0,0)
      v <- c(0,0,1)
    }
    # plane is perpendicular to z
    else if(p[1]==0 & p[2]==0){
      u <- c(1,0,0)
      v <- c(0,1,0)
    }
    # plane is parallel to y
    else if(sum(p2)==0){
      #print("plane is parallel to y")
      v <- c(0,1,0)
      u <- slvwagner::math_cross_product(p,v)
      u <- u/slvwagner::math_betrag(u)
    }
  }
  names(n) <- c("a","b","c")
  return(list(parameter = cbind(p=p,u=u,v=v),n = n,d=d))
}

#######################################
#' Calculate the an intersection point of three spears
#'
#' @name geo_interSec_3spheres
#' @description
#' Using numerical solver to calculate a intersecting point upon an initial guess.
#' @details
#' The spears will be calculated from the form (x - x0)^2 + (y - y0)^2 + (z - z0)^2 - r^2).
#' @param  param_matrix for each spear a vector with its coefficients is needed: c(x0,y0,z0,r) so the matrix will have rows and four columns
#' @param  initial_guess A vector with the intial guess c(x = 0,y = 0,z = 1)
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' vector with named coordinates
#' @examples
#' library(nleqslv)
#' params <- matrix(c(0 , 0, 0, 20.728,
#'                    15, 0, 0, 31.304,
#'                    0 ,10, 0, 26.576), ncol = 4, byrow = TRUE)
#' colnames(params) <- c("x0","y0","z0","r")
#' params
#' geo_interSec_3spheres(params, c(0,0,1))
#' @export

geo_interSec_3spheres<- function(param_matrix, initial_guess) {
  equations <- function(vars) {
    residuals <- apply(param_matrix, 1, function(params) {
      return((vars[1] - params[1])^2 + (vars[2] - params[2])^2 + (vars[3] - params[3])^2 - params[4]^2)
    })
    return(residuals)
  }

  # Solve the system of equations numerically
  solution <- nleqslv::nleqslv(initial_guess, equations)

  # Return the intersecting points
  return(c(x = solution$x[1], y = solution$x[2], z = solution$x[3]))
}

#######################################
#' Intersection of a plane and a line
#'
#' @name geo_intersec_plane_line
#' @description
#' Function returning intersecting point of a plane and a 3D-line if it exists.
#' @param  p position vector for the plane
#' @param  n normal vector of the plane
#' @param  s position vector of the line
#' @param  w direction vector of the line
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' named vector (Intersecting point)
#'
#' @examples
#' geo_intersec_plane_line(p = c(0,0,0),n = c(0,0,1),s = c(-1,-1,1), w = c(1,1,-1))
#' @export

geo_intersec_plane_line <- function(p, n, s, w) {
  dotProduct <- sum(n * w)
  # If the dot product is zero, the line is parallel to the plane and no intersection exists
  if (dotProduct == 0) {
    stop("The line is parallel to the plane. No intersection exists.")
  }
  # Calculate the vector from a point on the plane to a point on the line
  v <- s - p
  # Calculate the scalar value t for the line parametrization
  t <- -sum(n * v) / dotProduct
  # Calculate the intersection point
  Ipoint <- s + t * w
  names(Ipoint) <- c("x","y","z")
  return(Ipoint)
}

#######################################
#' Coordinate function of a plane
#'
#' @name geo_plane_fun
#' @description
#' Function returning a cas function of a plane defined by position vector \eqn{p} and the normal vector \eqn{n} of a plane.
#' @details
#' The calculation is done by Ryacas (yacas) and uses symbolic math. The package Ryacas and the software YACAS needs to be installed.
#' @details \url{http://www.yacas.org/}
#' @param  p position vector
#' @param  n normal vector of the plane
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Yacas function
#'
#' @examples
#' library(Ryacas)
#' geo_plane_fun(c(2,3,4), c(12,5,1)|>math_unit_vector())
#' @export

geo_plane_fun <- function(p,n){
  x <- paste0("x",1:3)|>
    Ryacas::ysym()

  EQ_Ebene <- Ryacas::ysym(n)*(x-Ryacas::ysym(p))
  EQ_Ebene <- EQ_Ebene[1]+EQ_Ebene[2]+EQ_Ebene[3]
  EQ_Ebene
}
