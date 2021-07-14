#' Get normal distributed overlay for faceted histogram
#'
#' @name plot_gausOverlayData
#' @description
#' To compare histograms, it is best practice to facet each (diagram) feature using ggplot.
#' To know whether the distribution is similar to the normal gaussian distribution, an overlay with the gaussian distribution can help.
#' Of corse the Q-Q Plot is a more appropriated tool to compare normal distribution and sample distribution.
#'
#' @details
#' The first argument is a (named) list of vectors or a (named) data frame to calculate the normal distribution for each vector,
#' where as the name is representing the feature name or the facet in the histogram. If no names are provided the function
#' paste("list_element",1:length(features)) will name the features.
#' The second argument is the number of bins to be used in the histogram.
#' The third argument is the ration or multiplier of, how many more data points are generated than bins are ploted in the histogram.
#' This ensures that for small bin count enough data points are available.
#' To be able to generate a faceted plot the bin width is important to set equal in the ggplot call
#' ggplot()+geom_histogram(aes(...),binwidth = 1). Check the examples for further information.
#' @param features list of vectors (features) or data frame
#' @param binwidth number to define the bin width, binwidth = 1
#' @param ratio number to define how many more data points as bins are generated, ratio = 5
#' @return returns tibble with coordinates x, y and feature name
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
#' df_gaus <- plot_gausOverlayData(df_data%>%as.list(),binwidth = binwidth, 10)
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

plot_gausOverlayData <- function(features, binwidth = 1, ratio = 5) {
  # check data type
  if(is.data.frame(features)|| is.list(features) & !is.null(features)){
    # change df to list
    if(is.data.frame(features)){
      features <- as.list(features)
    }# check for feature names
    if(is.null(names(features))){ # if the feature does not have a feature name, create one according to the list elemente entry
        names(features) <- paste("list_element",1:length(features))
    }
    data <- unlist(features)
    bins <- seq(floor(min(data)),ceiling(max(data)),binwidth/ratio)
    l_gaus <- list()
    for(i in 1:length(features)){
      hist <- hist(features[[i]], breaks = bins, plot = FALSE)#, main = paste(levels(df$Messungen)[i]))
      #Normalverteilung
      if(hist$equidist == TRUE){
        multiplier <- hist$counts / hist$density
        multiplier <- mean(multiplier, na.rm = TRUE)*ratio
      }else{writeLines("Error: \ncan not overlay a histogram with non equal bin distance")}
      gaus <- multiplier*dnorm(hist$breaks,mean = mean(features[[i]]), sd = sd(features[[i]]))
      l_gaus[[i]] <- data.frame(x= hist$breaks,
                                y = gaus,
                                feature = factor(rep(names(features)[i],length(gaus))))
      l_gaus[[i]]$y <- ifelse(is.na(l_gaus[[i]]$y),0,l_gaus[[i]]$y) #NA`s mit 0 ersetzen
    }
    df_temp <- l_gaus[[1]]
    if(length(l_gaus)>1){
      for (i in 2:length(features)) {
        df_temp <- rbind(df_temp,l_gaus[[i]])
      }
    }
    return(df_temp)
  }else{
    print("You need to supply a list or a data frame with the first argument wich is not NULL")
    }
}

