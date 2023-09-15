## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slvwagner)

## -----------------------------------------------------------------------------
c_slope <- 0.3
c_intercept <- 1

cbind(x = -10:10, y = lf_lf(-10:10,c_slope,c_intercept))|>
  plot(main = "lf_lf", type = "b", asp = 1)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(tidyverse)
tibble(y = rnorm(100,mean = -2.5))|>
  mutate(x = row_number(y),
         y_centered = signal_center(y))|>
pivot_longer(cols = c("y", "y_centered"))|>
ggplot(aes(x,value, color = name))+
  geom_point()+
  geom_hline(yintercept = 0)

## -----------------------------------------------------------------------------
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

m_grid <- cbind(m_grid,
                edge = find_edges_2D(m_grid[,"c_search"], n_col))

## ----echo=FALSE---------------------------------------------------------------
m_grid|>
  plot(col = m_grid[,"c_search"],
       xlim=c(0,n_col-1), ylim=c(0,n_row-1),
       # asp = 1,
       pch = 19,
       cex = 1.,
       main = "Input: c_search")

m_grid|>
  plot(col = m_grid[,"edge"],
       xlim=c(0,n_col-1), ylim=c(0,n_row-1),
       # asp = 1,
       pch = 19,
       cex = 1.,
       main = "Edge")


## -----------------------------------------------------------------------------
# Input
tbl_of_contents.Rmd

## -----------------------------------------------------------------------------
# Output with Table of contents and page break
r_toc_for_Rmd(tbl_of_contents.Rmd, toc_heading_string = "Inhaltsverzeichnis",create_nb = TRUE, create_top_link = TRUE , nb_front = TRUE, pagebreak_level = "2")|>
  write("test_.Rmd")

readLines("test_.Rmd")

