# slvwagner
My R library, including all functions I use very often.

Some function need yacas an simbolic solver: 
http://www.yacas.org/

## Content
### Math in R math_
Functions start with math_

-   Transformation from spherical to Cartesian coordinate system and back. 
-   Rotation Matrix in R and Ryacas (YACAS)
-   lf Linear function helpers 
-   math_ Quadrant and angle finder for 2D vector
-   math_ cross and dot vector product

### Utilities
Functions start with r_

-   r_ R utility functions, e.g. windows path to R compatible path
-   Create fast dictionary, convenient functions.

### Geometrical stuff 
Functions start with geo_

-   conic section 
-   slerp, 
-   Intersection of sphere and line, 
-   Intersecting point(s) of plane and line

### Math in yacas 
Functions start with math_
-   rotation matrix 2D
-   rotation matrix 3D
-   Plane coordinate function
-   Intersection plane and line


## Help pages 

cas_intersection_plane_line	CAS intersection of a plane and a line
cas_plane_fun	CAS coordinate function of a plane
cas_rot_matrix2d	CAS 2d rotation matrix
cas_rot_matrix3d	CAS 3d rotation matrix
dict_assign_key_values	Assigne key and value to fast dictionary
dict_exists_key	Check if key is in dictionary
dict_from_data.frame	Create a fast dictionary from data frame
dict_get_values	Get values from fast dictionary
dict_init	Initialize fast dictionary environment
dict_update	Update key/value pairs of fast dictionary
geo_conic_section_from_5points	Conic section from five point
geo_interSec_sph_line	Intersection point(s) of line and sphere.
geo_slerp	slerp by 3 points and a given radius.
math_angle_quadrant	Find quadrant(s) of vector(s)
math_angle_quadrant_vector	Find the angle of a vector
math_betrag	Magnitude of a vector
math_cart2sph	Cartesian to sperical coordinates
math_circle_from3points	Circle from 3 points
math_cross_product	Compute Vector Cross Product
math_dot_product	Compute the dot product of two vectors
math_inbetweenAngle	Angle between two vectors
math_lf	linear function with parameter mx+b
math_lf_df_mb	linear function but with data frame instead of single parameters
math_lf_fromPoints	linear function from 2 points
math_lf_intersect	Intersection point from two linear functions
math_lf_perpendicular	Find linear function perpendicular to linear function through given point
math_lf_rev_slope	find perpendicular slope to linear function
math_quadrant	Find quadrant(s) of vector(s)
math_quadrant_vector	Find quadrant of vector
math_rot_matrix2d	2d rotation matrix
math_rot_matrix3d	3d rotation matrix
math_rot_transform	Transform vector by a rotation matrix
math_sph2cart	Sperical to cartesian coordinates
math_unit_vector	Calculate unit vector
NPS	Get Net Promoter Score
NPS_type	Get NPS type
plot_gausOverlayData	Get normal distributed overlay for faceted histogram
r_colourise	Colourise text for display in the terminal.
r_is.defined	Check if variable is defined in R's global environment.
r_path	Convert Windows file path to R compatible file path
r_signif	Format number to defined signify(x,digits = 3)
r_win_path	Convert R path to windows compatible path
signal_center	Center Signal around zero (AC cupling)
stat_cpk	Process capability index
stat_norm_dist	Probability density function
