#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//////////////////////////////////////////////
//
//' @name find_edges_3D
//' @title Find edges in an evenly spaced 3D grid.
//' @description Checks all neighbour indices in a vector \code{c_search} for \code{TRUE}. If all neighbours are \code{TRUE} it is not an edge, else it is.
//' @param c_search Logical vector
//' @param n dimensions of 3D array c(nx, ny, nz)
//' @return logical vector if edge was found
//' @examples
//' n <- c(4L,3L,3L)
//' df_input <- expand.grid(
//'   x = 1:n[1],
//'   y = 1:n[2],
//'   z = 1:n[3])
//' df_input$search <- TRUE
//' df_input$edge <- find_edges_3D(df_input$search, n)
//' df_input$color <- ifelse(df_input$edge, "blue", "black")
//'
//' df_input
//' #df_input|>
//' #  rgl::plot3d(col = df_input$color, size = 15,aspect = "iso")
//' @export

// [[Rcpp::export]]
LogicalVector find_edges_3D(LogicalVector c_search, const IntegerVector n) {

  vector<int> dx_ = { -1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1};
  vector<int> dy_ = { -1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1};
  vector<int> dz_ = { -1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1};

  IntegerVector dx = wrap(dx_);
  IntegerVector dy = wrap(dy_);
  IntegerVector dz = wrap(dz_);

  // create result vectors
  LogicalVector c_result(Rcpp::clone(c_search));

  if(c_search.size() % (n[0]*n[1]) != 0) stop("Error: The size of the search vector is not a muliple of n");

  // The c_search vector has to be expanded to search for neighbours using the offset vectors.
  int z0 = (n[0] + 2) * (n[1] + 2);
  int y0 = n[0] + 2;
  LogicalVector search((n[0] + 2) * (n[1] + 2) * (n[2] + 2));

  // Fill in the values in the enlarged vector
  for (int ii = 0; ii < c_search.size(); ii++) {
    // compute x/y/y position from flat array index
    int z = (int)(ii / (n[0] * n[1]));
    int y = (int)((ii - z * n[0] * n[1]) / n[0]);
    int x = ii - z * n[0] * n[1] - y * n[0];
    // compute index for expanded array
    int index = z0 + y0 + y0 * y + z0 * z + x + 1;

    // Fill in values into expanded search vector
    search[index] = c_search[ii];

  }

  // check all neighbours and if there are all set result to false (no edge)
  for (int ii = 0; ii < c_search.size(); ii++) {
    // compute x/y/y position from flat array index
    int z = (int)(ii / (n[0] * n[1]));
    int y = (int)((ii - z * n[0] * n[1]) / n[0]);
    int x = ii - z * n[0] * n[1] - y * n[0];
    // compute index for expanded array
    int index = z0 + y0 + y0 * y + z0 * z + x + 1;

    // Scan the neighbours only if needed
    if (c_search[ii]) {
      int neighbours = 0;

      // scan neighbours by offset vector
      for (int jj = 0; jj < (int)dx.size(); jj++) {
        // compute indices for expanded grid with offsets
        index = z0 + y0 + y0 * (y + dy[jj]) + z0 * (z + dz[jj]) + (x + dx[jj]) + 1;

        if (search[index]) {
          neighbours++;
        }
      }

      // if all neighbours are there it is considered an edge
      if (neighbours == 8+9+9) {
        c_result[ii] = false;
      }
    }
  }
  return c_result;
}


/*** R

library(rgl)
n <- c(6L,5L,4L)
df_input <- expand.grid(
  x = 1:n[1],
  y = 1:n[2],
  z = 1:n[3])

df_input$search <- TRUE
df_input$edge <- find_edges_3D(df_input$search, n)
df_input$color <- ifelse(df_input$edge, "blue", "black")

df_input
df_input|>
  plot3d(col = df_input$color, size = 15,aspect = "iso")

*/
