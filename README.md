# slvwagner
My R library, including all functions I use very often.

Some function need yacas a symbolic solver: \
http://www.yacas.org/

## Content

### Geometrical stuff 
Functions start with geo_

-   conic section 
-   slerp 
-   Intersection of sphere and line, 
-   Intersecting point(s) of plane and line
-   convert plane equation (coordinate form) into parametric from. Get position vector and tow orientation vectors.  
-   intersection point from three globs upon an initial guess

### Math 
Functions start with math_

-   Transformation from spherical to Cartesian coordinate system and back. 
-   Rotation Matrix in R and Ryacas (YACAS)
-   lf Linear function helpers 
-   Quadrant and angle finder for 2D vector
-   cross and dot vector product
-   Magnitude of vector 
-   In-between angel of two vectors
-   Get polynomial from roots 
-   Round polynomial equation
-   Generate non linear Vector with start, end and vector length

#### Math in yacas 
Functions start with yac_

-   rotation matrix 2D
-   rotation matrix 3D
-   Plane coordinate function
-   Intersection plane and line

#### Linear function helper
Functions start with lf_

- simple linear function 
- perpendicular to function 
- intersection point of two functions
- linear function from two points

#### signal
Functions start with signal_

-   Center Signal around zero (AC cupling)


#### statistical 
Functions start with stat_

-   Process capability index (CPK)
-   Probability density function

### busines
- NPS (net promoter score)

### Utilities

#### Plot
Functions start with plot_

-    Get normal distributed overlay for faceted histograms

#### System 
Functions start with r_

-   r_ R utility functions, e.g. windows path to R compatible path

#### Fast dictionary
Function start with dict_

-   Create fast dictionary, convenience functions.

