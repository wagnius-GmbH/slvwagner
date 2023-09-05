## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slvwagner)

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
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


