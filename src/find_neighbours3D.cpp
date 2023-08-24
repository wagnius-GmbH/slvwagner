#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//////////////////////////////////////////////
//
//' @name find_edges_3D
//' @title Find edges in an evenly spaced 3D grid.
//' @description Checks all neighbour indices in a vector \code{c_search} for \code{TRUE}. If all neighbours are \code{TRUE} it is not an edge, else it is.
//' @param c_search Logical vector
//' @param n dimension of 3D array c(nx, ny, nz)
//' @param offset DataFrame with dx,dy, dz offsets
//' @return logical vector if edge was found
//' @examples
//' n_col <- 7
//' n_row <- 4
//' c_search <- c(
//'   0,1,0,0,0,1,0,
//'   1,1,1,1,1,1,1,
//'   0,1,0,1,1,1,1,
//'   0,0,1,1,1,1,1
//'   )
//' m_grid <- expand.grid(x = 0:(n_col-1), y = 0:(n_row-1))|>
//'   as.matrix()|>
//'   cbind(c_search)
//' m_grid|>
//' head()
//' m_grid|>
//' plot(col = m_grid[,"c_search"],
//'   xlim=c(0,n_col-1), ylim=c(0,n_row-1),
//'   pch = 19,
//'   cex = 2.5,
//'   main = "Input: c_search")
//'
//' m_grid <- cbind(m_grid,
//'                 edge = find_edges_3D(m_grid[,"c_search"], n_col))
//'
//' m_grid|>
//'   head()
//' m_grid|>
//'   plot(col = m_grid[,"edge"],
//'        xlim=c(0,n_col-1), ylim=c(0,n_row-1),
//'        pch = 19,
//'        cex = 2.5,
//'        main = "Edge")
//' @export

// [[Rcpp::export]]
LogicalVector find_edges_3D(const LogicalVector c_search, const IntegerVector n, const DataFrame offset) {
  IntegerVector dx = offset[0];
  IntegerVector dy = offset[1];
  IntegerVector dz = offset[2];

  // create result vectors
  LogicalVector c_result(Rcpp::clone(c_search));

  // Create a search vector, the c_search vector has to be expanded to search for neighbours using the offset vectors.
  // The values need to be copied to the expanded vector.

  int z0 = (n[0]+2)*(n[1]+2);
  int y0 = n[0]+2+1;

  LogicalVector search((n[0]+2)*(n[1]+2)*(n[2]+2));

  // Fill in the values in the enlarged vector
  for (int ii = 0; ii < c_search.size(); ii++) {
    int z = (int)(ii / (n[0] * n[1]));
    int y = (int)((ii - z * n[0] * n[1]) / n[0]);
    int x = ii - z * n[0] * n[1] - y * n[0];
    int index = z0 + y0 + y0*y + z0*z + x + 1;

    // Fill in values into expanded search vector
    search[index] = c_search[ii];
  }

  Rcout << search << endl;

  // find neighbours and if there are set c_result to true
  for (int ii = 0; ii < c_search.size(); ii++) {
    // Scan the neighbours only if needed
    if(c_search[ii]){
      int neighbours = 0;
      // compute indices for c_search grid
      int z = (int)(ii / (n[0] * n[1]));
      int y = (int)((ii - z * n[0] * n[1]) / n[0]);
      int x = ii - z * n[0] * n[1] - y * n[0];

      // scan neighbours by offset vector
      for(int jj = 0; jj < dx.size(); jj++){
        // compute indices for expanded grid but with dx,dy offsets
        int index = z0 + y0 + y0*(y+dy[jj]) + z0*(z+dz[jj]) + (x+dx[jj]) + 1;
        //
        if(search[index]){
           neighbours++;
         }
      }
      // if all neighbours are there it is considered an edge
      if(neighbours == 26) {
         c_result[ii] = false;
      }
    }
  }
  return c_result;
}


/*** R

library(rgl)
library(tidyverse)

c_offset3D <- expand.grid(x = -1:1,
                          y = -1:1,
                          z = -1:1)

c_offset3D <- c_offset3D[!(c_offset3D[,"x"] == 0 & c_offset3D[,"y"] == 0 & c_offset3D[,"z"] == 0),]
c_offset3D

c_offset3D|>
  plot3d(size = 10, aspect = "iso")


test_data <- expand.grid(x = 0:2,
                         y = 0:2,
                         z = 0:2)|>
  bind_rows(expand.grid(x = 5:10,
                        y = 5:10,
                        z = 5:10),
            expand.grid(x = 7:10,
                        y = 5:11,
                        z = 8:12),
            tibble(x = c(-1,0,1, 0,0, 0,0)+5,
                   y = c( 0,0,0,-1,1, 0,0)+3,
                   z = c( 0,0,0, 0,0,-1,1)+2)
  )|>
  mutate(search = T)|>
  distinct()

s_test_data <- test_data|>
  summarise(max(x),min(x),
            max(y),min(y),
            max(z),min(z),)

s_test_data

df_data <- expand.grid(x = s_test_data$`min(x)` :s_test_data$`max(x)`,
                       y = s_test_data$`min(y)` :s_test_data$`max(y)`,
                       z = s_test_data$`min(z)` :s_test_data$`max(z)`)

df_data <- df_data|>
  left_join(test_data, by = join_by(x, y, z))|>
  as_tibble()|>
  mutate(index = 0:(nrow(df_data)-1))

df_data|>
  filter(!is.na(search))|>
  plot3d(size = 10, aspect = "iso")

## Index calculation
s_df_data <- df_data|>
  summarise(x  = max(x)-min(x),
            y = max(y)-min(y),
            z  = max(z)-min(z)
  )

s_df_data

find_edges_3D(!is.na(df_data$search), c(s_df_data$x,s_df_data$y,s_df_data$z), c_offset3D )

df_data|>
  filter(!is.na(search))|>
  plot3d(size = 10, aspect = "iso")

*/
