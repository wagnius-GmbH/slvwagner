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
//' @return logical vector if edge was found
//' @examples
//' n <- c(4L,3L,3L)
//' df_input <- expand.grid(
//'   x = 1:n[1],
//'   y = 1:n[2],
//'   z = 1:n[3])|>
//'   mutate(color = "blue",
//'          search= T,
//'          edge = find_edges_3D(search, n))
//' @export

// [[Rcpp::export]]
LogicalVector find_edges_3D(LogicalVector c_search, const IntegerVector n) {

  vector<int> dx_ = { -1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1};
  vector<int> dy_ = { -1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1};
  vector<int> dz_ = { -1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1};

  IntegerVector dx = wrap(dx_);
  IntegerVector dy = wrap(dy_);
  IntegerVector dz = wrap(dz_);

  // Rcout
  // << "dx: " << dx <<  endl
  // << "dy: " << dy <<  endl
  // << "dz: " << dz <<  endl
  // <<  endl;

  // create result vectors
  LogicalVector c_result(Rcpp::clone(c_search));

  // The c_search vector has to be expanded to search for neighbours using the offset vectors.
  int z0 = (n[0] + 2) * (n[1] + 2);
  int y0 = n[0] + 2;
  LogicalVector search((n[0] + 2) * (n[1] + 2) * (n[2] + 2));

  // Rcout
  // << "z0: " << z0
  // << " y0: " << y0
  // << " search.size(): " << search.size()
  // <<  endl;

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


    // if(c_search[ii]){
    //   Rcout
    //   << "Index: " << ii
    //   << " ( x: " << x
    //   << "  y: " << y
    //   << "  z: " << z
    //   << " )"
    //   << " ( Index.expanded: " << index
    //   << " )"
    //   << endl;}
  }


  // for (auto val : c_search) Rcout << val << " / ";
  // Rcout << endl;
  // for (int ii = 0 ; ii < search.size(); ii++) {
  //    Rcout << "index.expanded: " << ii << " search[index.expanded] " << search[ii] << endl;
  // }
  // Rcout << endl;


  // check all neighbours and if there are all set result to false (no edge)
  for (int ii = 0; ii < c_search.size(); ii++) {
    // compute x/y/y position from flat array index
    int z = (int)(ii / (n[0] * n[1]));
    int y = (int)((ii - z * n[0] * n[1]) / n[0]);
    int x = ii - z * n[0] * n[1] - y * n[0];
    // compute index for expanded array
    int index = z0 + y0 + y0 * y + z0 * z + x + 1;

    // Rcout
    //    << "\n\n"
    //    << "Index: " << ii
    //    << " ( x: " << x
    //    << "  y: " << y
    //    << "  z: " << z
    //    << " )"
    //    << " ( Index.expanded: " << index
    //    << " )"
    //    << "\n********************"
    //    << endl;

    // Scan the neighbours only if needed
    if (c_search[ii]) {
      int neighbours = 0;

      // scan neighbours by offset vector
      for (int jj = 0; jj < (int)dx.size(); jj++) {
        // compute indices for expanded grid with offsets
        index = z0 + y0 + y0 * (y + dy[jj]) + z0 * (z + dz[jj]) + (x + dx[jj]) + 1;
        // Rcout
        //    << " ( dx: " << dx[jj]
        //    << "  dy: " << dy[jj]
        //    << "  dz: " << dz[jj]
        //    << " )"
        //    << " Index: " << index
        //    << "    Serach[index]: " << search[index]
        //    << endl;

        //
        if (search[index]) {
          neighbours++;
          // Rcout
          // << "neighbours: " << neighbours
          // << endl;
        }
      }

      // if all neighbours are there it is considered an edge
      if (neighbours == 8+9+9) {
        c_result[ii] = false;
        // Rcout
        // << " ( x: " << x
        // << "  y: " << y
        // << "  z: " << z
        // << " )"
        // << " index: " << z0 + y0 + y0 * y + z0 * z + x + 1
        // << "    Serach[index]: " << search[index]
        // << endl;
      }
    }
  }
  return c_result;
}


/*** R

library(rgl)
library(tidyverse)
nx <- 4L; ny <- 3L; nz <- 3L

n <- c(4L,3L,3L)

df_input <- expand.grid(x = 1:n[1],
                        y = 1:n[2],
                        z = 1:n[3])|>
  mutate(color = "blue",
         search= T,
         edge = find_edges_3D(search, n)
         )




df_expanded <- expand.grid(x = 0:(nx+1),
                           y = 0:(ny+1),
                           z = 0:(nz+1))|>
  mutate(index = row_number()-1)|>
  left_join(df_input)|>
  mutate(color = if_else(is.na(color), "black", "blue"),
         search = color == "blue")
df_expanded

df_input <- df_input|>
  mutate(index = row_number()-1)

# Plot data
z0 <- (nx+2)*(ny+2)
df_expanded[0:z0,"color"] <- "red"
y0 <- nx+2+1
df_expanded[(z0+1):(z0+y0),"color"] <- "green"
y_add <- 2
df_expanded[(z0+y0+1+nx):(z0+y0+nx+y_add),"color"] <- "pink"
df_expanded|>
  plot3d(col = df_expanded$color, aspect = "iso")
df_expanded|>
  text3d(text = (df_expanded$index),col = df_expanded$color, size = 100, aspect = "iso", add = T)


edges <- find_edges_3D( df_input$search, n)

df_input$search
edges

# c_offset3D <- c_offset3D[!(c_offset3D[,"x"] == 0 & c_offset3D[,"y"] == 0 & c_offset3D[,"z"] == 0),]
# c_offset3D$x|>str_c(collapse = ",")|>writeClipboard()
# c_offset3D$y|>str_c(collapse = ",")|>writeClipboard()
# c_offset3D$z|>str_c(collapse = ",")|>writeClipboard()
#
# c_offset3D|>
#   plot3d(size = 10, aspect = "iso")
#
#
# test_data <- expand.grid(x = 0:2,
#                          y = 0:2,
#                          z = 0:2)|>
#   bind_rows(expand.grid(x = 5:10,
#                         y = 5:10,
#                         z = 5:10),
#             expand.grid(x = 7:10,
#                         y = 5:11,
#                         z = 8:12),
#             tibble(x = c(-1,0,1, 0,0, 0,0)+5,
#                    y = c( 0,0,0,-1,1, 0,0)+3,
#                    z = c( 0,0,0, 0,0,-1,1)+2)
#   )|>
#   mutate(search = T)|>
#   distinct()
#
# s_test_data <- test_data|>
#   summarise(max(x),min(x),
#             max(y),min(y),
#             max(z),min(z),)
#
# s_test_data
#
# df_data <- expand.grid(x = s_test_data$`min(x)` :s_test_data$`max(x)`,
#                        y = s_test_data$`min(y)` :s_test_data$`max(y)`,
#                        z = s_test_data$`min(z)` :s_test_data$`max(z)`)
#
# df_data <- df_data|>
#   left_join(test_data, by = join_by(x, y, z))|>
#   as_tibble()|>
#   mutate(index = 0:(nrow(df_data)-1),
#          search = !is.na(search),
#          color = if_else(search,"blue", "pink"))
#
# df_data|>
#   plot3d(col = df_data$color, size = 10, aspect = "iso")
#
# ## Index calculation
# s_df_data <- df_data|>
#   summarise(dx  = max(x)-min(x),
#             dy = max(y)-min(y),
#             dz  = max(z)-min(z)
#   )
#
# s_df_data
#
# find_edges_3D(df_data$search, c(s_df_data$dx,s_df_data$dy,s_df_data$dz))
#
#
# df_data <- df_data|>
#   mutate(color = if_else(find_edges_3D(df_data$search, c(s_df_data$dx,s_df_data$dy,s_df_data$dz)),
#                          "blue", "pink")
#          )
#
# df_data|>
#   plot3d(col = df_data$color, size = 10, aspect = "iso")
#
#


*/
