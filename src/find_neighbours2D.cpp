#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// R like seq() function to generate a sequence of numbers
template <typename T>
std::vector<T> seq_(T start, T end, T step = T(1)) {
  std::vector<T> sequence;

  if (step == T(0)) {
    std::cerr << "Error: Step size cannot be zero." << std::endl;
    return sequence;
  }

  if ((step > T(0) && start > end) || (step < T(0) && start < end)) {
    std::cerr << "Error: Invalid start, end, and step combination." << std::endl;
    return sequence;
  }

  for (T i = start; (step > T(0)) ? (i <= end) : (i >= end); i += step) {
    sequence.push_back(i);
  }

  return sequence;
}

template <typename t>
std::vector<std::vector<t>> expandGrid(const std::vector<std::vector<t>>& inputvectors) {
  size_t numvectors = inputvectors.size();

  // calculate the total number of combinations
  size_t totalcombinations = 1;
  for (const auto& vec : inputvectors) {
    totalcombinations *= vec.size();
  }

  // reserve memory for the result vector
  std::vector<std::vector<t>> result;
  result.reserve(totalcombinations);

  // generate the combinations
  for (size_t i = 0; i < totalcombinations; ++i) {
    std::vector<t> combination(numvectors);
    size_t temp = i;
    for (size_t j = 0; j < numvectors; ++j) {
      combination[j] = inputvectors[j][temp % inputvectors[j].size()];
      temp /= inputvectors[j].size();
    }
    result.push_back(std::move(combination));  // move the combination into the result
  }
  return result;
}

//////////////////////////////////////////////
//' find_edges_2D
//' @name find_edges_2D
//' @title find_edges_2D
//' @description Finds edges in an evenly spaced 2D grid.
//' @details Checks all neighbour indices in a vector \code{c_search} for \code{TRUE}. If all neighbours are \code{TRUE} it is not an edge, else it is.
//' @param c_search Logical vector
//' @param n_col x direction of the matrix
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
//'                 edge = find_edges_2D(m_grid[,"c_search"], n_col))
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
LogicalVector find_edges_2D(const LogicalVector c_search, const int n_col) {
  // coput the number of columns
  int n_row = 0;
  if(c_search.size() % n_col == 0) {
    n_row = (int)(c_search.size()/n_col);
  } else stop("Error: The size of the search vector is not a muliple of n_row");

  // create result vectors
  LogicalVector c_result(Rcpp::clone(c_search));

  // Create a 2D vector for the offset vector
  IntegerVector dx = IntegerVector::create(
    -1, 0, 1,-1,
    1,-1, 0, 1
  );
  IntegerVector dy = IntegerVector::create(
    -1,-1,-1, 0,
    0, 1, 1, 1
  );

  // Create a search vector, the c_search vector has to be expanded to search for neighbours using the offset vectors.
  // The values need to be copied to the expanded vector.
  LogicalVector search((n_col+2)*2 + (2*n_row) + (n_col*n_row));

  // Fill in the values in the enlarged vector
  for (int ii = 0; ii < c_search.size(); ii++) {
    // compute indices for c_search grid
    int j = ii % n_col; // Compute the column index
    int i = ii / n_col;  // Compute the row    index
    // compute indices for expanded grid
    int index = (n_col) + ((i + 1) * 3) + (i * (n_col-1)) + j;

    // Fill in values into expanded search vector
    search[index] = c_search[ii];
  }

  // find neighbours and if there are set c_result to true
  for (int ii = 0; ii < c_search.size(); ii++) {
    // Scan the neighbours only if needed
    if(c_search[ii]){
      int neighbours = 0;
      // compute indices for c_search grid
      int j = ii % n_col; // Compute the column index
      int i = ii / n_col;  // Compute the row    index

      // scan neighbours by offset vector
      for(int jj = 0; jj < dx.size(); jj++){
        // compute indices for expanded grid but with dx,dy offsets
        int index = (n_col)+((i + 1 + dy[jj]) * 3) + ((i + dy[jj]) * (n_col - 1)) + j + dx[jj];
        //
        if(search[index]){
          neighbours++;
        }
      }
      // set if
      if(neighbours == 8) {
        c_result[ii] = false;
      }
    }
  }
  return c_result;
}

/*** R
#################################################################
library(slvwagner)

n_col <- 7
n_row <- 4

c_search <- c(
  0,1,0,0,0,1,0,
  1,1,1,1,1,1,1,
  0,1,0,1,1,1,1,
  0,0,1,1,1,1,1
  )

m_grid <- expand.grid(x = 0:(n_col-1), y = 0:(n_row-1))|>
  as.matrix()|>
  cbind(c_search)

m_grid|>
  head()

m_grid|>
  plot(col = m_grid[,"c_search"],
       xlim=c(0,n_col-1), ylim=c(0,n_row-1),
       # asp = 1,
       pch = 19,
       cex = 2.5,
       main = "Input: c_search")

m_grid <- cbind(m_grid,
                edge = find_edges_2D(m_grid[,"c_search"], n_col))

m_grid|>
  head()

m_grid|>
  plot(col = m_grid[,"edge"],
       xlim=c(0,n_col-1), ylim=c(0,n_row-1),
       # asp = 1,
       pch = 19,
       cex = 2.5,
       main = "Edge")

*/









