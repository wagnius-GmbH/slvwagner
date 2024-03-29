# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' find_edges_2D
#' @name find_edges_2D
#' @title find_edges_2D
#' @description Finds edges in an evenly spaced 2D grid.
#' @details Checks all neighbour indices in a vector \code{c_search} for \code{TRUE}. If all neighbours are \code{TRUE} it is not an edge, else it is.
#' @param c_search Logical vector
#' @param n_col x direction of the matrix
#' @return logical vector if edge was found
#' @examples
#' n_col <- 7
#' n_row <- 4
#' c_search <- c(
#'   0,1,0,0,0,1,0,
#'   1,1,1,1,1,1,1,
#'   0,1,0,1,1,1,1,
#'   0,0,1,1,1,1,1
#'   )
#' m_grid <- expand.grid(x = 0:(n_col-1), y = 0:(n_row-1))|>
#'   as.matrix()|>
#'   cbind(c_search)
#' m_grid|>
#' head()
#' m_grid|>
#' plot(col = m_grid[,"c_search"],
#'   xlim=c(0,n_col-1), ylim=c(0,n_row-1),
#'   pch = 19,
#'   cex = 2.5,
#'   main = "Input: c_search")
#'
#' m_grid <- cbind(m_grid,
#'                 edge = find_edges_2D(m_grid[,"c_search"], n_col))
#'
#' m_grid|>
#'   head()
#' m_grid|>
#'   plot(col = m_grid[,"edge"],
#'        xlim=c(0,n_col-1), ylim=c(0,n_row-1),
#'        pch = 19,
#'        cex = 2.5,
#'        main = "Edge")
#' @export
NULL

find_edges_2D <- function(c_search, n_col) {
    .Call(`_slvwagner_find_edges_2D`, c_search, n_col)
}

#' @name find_edges_3D
#' @title Find edges in an evenly spaced 3D grid.
#' @description Checks all neighbour indices in a vector \code{c_search} for \code{TRUE}. If all neighbours are \code{TRUE} it is not an edge, else it is.
#' @param c_search Logical vector
#' @param n dimensions of 3D array c(nx, ny, nz)
#' @return logical vector if edge was found
#' @examples
#' n <- c(6L,5L,4L)
#' df_input <- expand.grid(
#'   x = 1:n[1],
#'   y = 1:n[2],
#'   z = 1:n[3])
#' df_input$search <- TRUE
#' df_input$edge <- find_edges_3D(df_input$search, n)
#' df_input$color <- ifelse(df_input$edge, "blue", "black")
#'
#' df_input
#' df_input|>
#'   rgl::plot3d(col = df_input$color, size = 15,aspect = "iso")
#'
#' @export
NULL

find_edges_3D <- function(c_search, n) {
    .Call(`_slvwagner_find_edges_3D`, c_search, n)
}

#' calc_roots_from_seeds_C
#' @name calc_roots_from_seeds_C
#' @title calc_roots_from_seeds_C
#' @description Create Student data frame.
#' @param seeds seeds
#' @param roots roots from polynom equation
#' @param poly polynomial coefficients
#' @examples
#' library(Rcpp)
#' df <- expand.grid(re = seq(0,10,0.1), im = seq(-10,0,0.1))
#' seeds <- complex(df$re,df$im)
#' calc_roots_from_seeds_C(seeds, c(0.2,2.5,3+1i), 1:10)
#' @export
NULL

#' expandGridComplex
#' @name expandGridComplex
#' @title expandGridComplex
#' @description
#' Create Student data frame with all possible variants of a complex vector.
#' @param x complex vector
#' @param epsilon consider the imaginary value as zero if smaller then \code{epsilon}
#' @return
#' returns a complex vector
#' @examples
#' expandGridComplex(complex(real = 1:100, imag = rnorm(100)))
#' @export
NULL

calc_roots_from_seeds_C <- function(seeds, roots, poly) {
    .Call(`_slvwagner_calc_roots_from_seeds_C`, seeds, roots, poly)
}

expandGridComplex <- function(x, epsilon = 1e-10) {
    .Call(`_slvwagner_expandGridComplex`, x, epsilon)
}

