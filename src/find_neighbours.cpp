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

// R like expand.grid() function to generate combinations
template <typename T>
std::vector<std::vector<T>> expandGrid(const std::vector<std::vector<T>>& inputVectors) {
  size_t numVectors = inputVectors.size();
  std::vector<std::vector<T>> result;

  // Calculate the total number of combinations
  size_t totalCombinations = 1;
  for (const auto& vec : inputVectors) {
    totalCombinations *= vec.size();
  }

  // Generate the combinations
  for (size_t i = 0; i < totalCombinations; ++i) {
    std::vector<T> combination(numVectors);
    size_t temp = i;
    for (size_t j = 0; j < numVectors; ++j) {
      combination[j] = inputVectors[j][temp % inputVectors[j].size()];
      temp /= inputVectors[j].size();
    }
    result.push_back(combination);
  }

  return result;
}

//////////////////////////////////////////////
//' math_findNeighbours_2D
//' @name math_findNeighbours_2D
//' @title math_findNeighbours_2D
//' @description checks if all neighbour indices are there to detect an edge in a matrix
//' @param c_search flat search vector to creat the matrix(c(),n_row,byrow = T) row wise sorted
//' @param n_col x direction of the matrix
//' @examples
//' n_col <- 7
//'  c_flat <- c(
//'   0,1,0,0,0,1,0,
//'   1,1,1,1,1,1,1,
//'   0,1,0,1,1,1,1,
//'   0,0,1,1,1,1,1)
//' m <- c_flat|>
//'   matrix(ncol = n_col, byrow = T)
//' m
//' df_data <- expand.grid(x = 1:n_col,  y = 1:4)
//' df_data$search <- c_flat == T
//' df_data$edge <- math_findNeighbours_2D(c_flat,n_col)
//' df_data$search == math_findNeighbours_2D(c_flat,n_col)
//' plot(df_data$x, df_data$y, col = df_data$search, main = "Search")
//' plot(df_data$x, df_data$y, col = df_data$edge, main = "Edges")
//'
//' @export

// [[Rcpp::export]]
LogicalVector math_findNeighbours_2D(const LogicalVector c_search, const int n_col) {

  int n_row = 0;

  if(c_search.size() % n_col == 0) {
    n_row = (int)(c_search.size()/n_col);
  } else stop("Error: The size of the search vector is not a muliple of n_row");

  // create result vectors
  LogicalVector c_result(Rcpp::clone(c_search));

  // Create a 2D vector for the x/y positions to be inspected
  // The values to be inspect are bigger n_row +1  and n_col + 1 bigger than the grid. Scanning also the edges
  std::vector<std::vector<int>> position;
  position.push_back(seq_(-1, n_row));
  position.push_back(seq_(-1, n_col));
  // create expanded grid
  std::vector<std::vector<int>> positions_xy = expandGrid(position);

  // Create a 2D vector for the offset vector
  IntegerVector dx = IntegerVector::create(
    -1, 0, 1,-1,
    1,-1, 0, 1
  );
  IntegerVector dy = IntegerVector::create(
    -1,-1,-1, 0,
    0, 1, 1, 1
  );

  //Create a search vector
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
n_col <- 7
c_flat <- c(
  0,1,0,0,0,1,0,
  1,1,1,1,1,1,1,
  0,1,0,1,1,1,1,
  0,0,1,1,1,1,1
  )
m <- c_flat|>
  matrix(ncol = n_col, byrow = T)
m
df_data <- expand.grid(x = 1:n_col,  y = 1:4)
df_data$search <- c_flat == T
df_data$edge <- math_findNeighbours_2D(c_flat,n_col)
df_data$search == math_findNeighbours_2D(c_flat,n_col)
plot(df_data$x, df_data$y, col = df_data$search, main = "Search")
plot(df_data$x, df_data$y, col = df_data$edge, main = "Edges")


library(tidyverse)
x_def <- c(0,29)
y_def <- c(0,29)

c_offset <- c(0,0)

df_data <- expand.grid(x = x_def[1]:x_def[2],
                       y = y_def[1]:y_def[2])

df_test <- df_data |>
  as_tibble() |>
  filter((x %in% c(2:4) & y %in% c(2:4))
  )|>
  mutate(see = "Hallwiler")

df_test$see[c(1,3,7,9)] <- NA

x_def <- x_def+c_offset[1]
y_def <- y_def+c_offset[2]

df_data <- df_data |>
  left_join(df_data |>
              as_tibble() |>
              filter((between(x, 6, 10) & between(y, 3, 10))|
                       (between(x, 20, 1000) & between(y, 20, 1000))

              )|>
              mutate(see = "Hallwiler")|>
              bind_rows(df_test),
            by = join_by(x, y)
  )|>
  as_tibble()|>
  mutate(x = x+c_offset[1],
         y = y+c_offset[2],
         see = !is.na(see))

df_data$see[1] <- T
df_data$see[6] <- T
summary(df_data)

######################################################################################

df_data <- df_data|>
  mutate(edge = math_findNeighbours_2D(df_data$see,diff(x_def)+1))

p <- df_data|>
  ggplot(aes(x,y))+
  geom_point(data = df_data|>filter(edge == T),
             aes(shape = edge),
             size = 6)+
  geom_point(aes(color = see), size = 3)+
  coord_fixed()+
  scale_x_continuous(breaks = x_def[1]:x_def[2])+
  scale_y_continuous(breaks = y_def[1]:y_def[2])+
  geom_vline(xintercept = x_def[1]:x_def[2])+
  geom_hline(yintercept = y_def[1]:y_def[2])+
  theme_light()+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        plot.background=element_blank())+
  labs(title = "findLakeEdgesRcpp")

print(p)
*/
