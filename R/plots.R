#' Get normal distributed overlay for faceted histogram
#'
#' @name get_gausOverlay
#' @description
#' To compare histograms, it is best practice to facet each feature using ggplot.
#' To know whether the distribution is similar to the normal gaussian distribution an overlay with the gaussian distribution can help.
#' @details
#' The first argument is a named list of vectors to calculate the normal distribution for each vector,
#' where as the name is representing the feature name or the facet in the histogram.
#' The second argument is the number of bins to be used in the histogram.
#' The third argument is the ration or multiplier of, how many more data points are generated.
#' This ensures that for small bin enough data points are available.
#' @param l_feature named list of vectors (features)
#' @param binwidth number to define the bin width, binwidth = 1
#' @param ratio number to define how many more data points as bins are generated, ratio = 5
#' @return tibble with coordinates x,y and feature name
#' @examples
#' library(tidyverse)
#' # create some data to Test
#' x <- 1:1000
#' df_data <- tibble(y1 = rnorm(x, mean = 7.8, sd = 2.1),
#'                   y2 = rnorm(x, mean = -3, sd = 1.2))%>%
#'                   bind_cols(tibble(y3 = rnorm(1:(length(x)-200), mean = 8, sd = 1.2))%>%
#'                   bind_rows(tibble(y3 = rnorm(1:200,             mean = 0, sd = 0.8))))
#' # create transformed version of data to plot
#' df_transformed <- df_data%>%
#' pivot_longer(cols = starts_with("y"))%>%
#' rename(feature = name)%>%
#' mutate(feature = factor(feature))%>%
#' group_by(feature)
#'
#' # find the min and max values of all data an create a vector with defined bin size
#' binwidth = 1
#' c_bins <- seq(floor(min(df_transformed$value)),ceiling(max(df_transformed$value)),binwidth)
#' df_gaus <- get_gausOverlay(df_data%>%as.list(),binwidth = binwidth, 10)
#'
#' # facetted histogramms with normal distributed line overlay
#' df_transformed%>%
#' ggplot()+
#'    geom_histogram(aes(value), binwidth = binwidth,fill = "lightgreen", color = "black")+
#'    geom_line(data = df_gaus, aes(x,y), color = "blue", size = 1.2)+
#'    facet_grid(feature~.)+
#'    scale_x_continuous(breaks = c_bins)
#'
#' @export

get_gausOverlay <- function(l_feature, binwidth = 1, ratio = 5) {
  data <- unlist(l_feature)
  bins <- seq(floor(min(data)),ceiling(max(data)),binwidth/ratio)
  l_gaus <- list()
  for(i in 1:length(l_feature)){
    hist <- hist(l_feature[[i]], breaks = bins, plot = FALSE)#, main = paste(levels(df$Messungen)[i]))
    #Normalverteilung
    if(hist$equidist == TRUE){
      multiplier <- hist$counts / hist$density
      multiplier <- mean(multiplier, na.rm = TRUE)*ratio
    }else{writeLines("Error: \ncan not overlay a histogram with non equal bin distance with normal distribution")}
    gaus <- multiplier*dnorm(hist$breaks,mean = mean(l_feature[[i]]), sd = sd(l_feature[[i]]))
    l_gaus[[i]] <- tibble(x= hist$breaks,
                          y = gaus,
                          feature = factor(rep(names(l_feature)[i],length(gaus))))%>%
      mutate(y = if_else(is.na(y),0,y)) #NA`s mit 0 ersetzen
  }
  bind_rows(l_gaus)
}
