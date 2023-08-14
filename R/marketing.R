####################################
#' Get NPS type
#'
#' @name NPS_type
#' @param df_data data frame with data
#' @param rating string identifying the column of the rating
#' @param df_levels data frame containing range for each type (Promoters, Passives, Detractors)
#' df_levels <- tibble::tibble(Detractors = c(1,6),
#'                     Passives   = c(7,8),
#'                     Promoters  = c(9,10))
#' @return data frame \code{df_data} with new feature (column = typ_nps)
#' @examples
#' library(tidyverse)
#' # Test data
#' df_data <- tibble::tibble( rating = c(1:10,rep(10,5)),
#'                    c_group = c(rep("A",10),rep("B",5)))
#' df_levels <- tibble::tibble(Detractors= c(1,6),Passives= c(7,8), Promoters= c(9,10))
#' NPS_type(df_data, df_levels = df_levels)
#'
#' df_data <- tibble::tibble(c_group  = c(letters[1:10],rep("a",5)),
#'                   rating = c(1:5,rep(5,10)))
#' df_levels <- tibble::tibble(Detractors= c(1,3),Passives= c(4,4.5), Promoters= c(4.5,5))
#' NPS_type(df_data, df_levels = df_levels)
#' @export

NPS_type <- function(df_data, rating = "rating", df_levels) {
  # Create feature Promoters, Passives and Detractors
  df_data <- df_data|>
    dplyr::mutate(Detractors = between(rating, df_levels$Detractors[1], df_levels$Detractors[2])|>
                    dplyr::if_else("Detractors","x_x")|>na_if("x_x"),
                  Passives   = between(rating, df_levels$Passives[1], df_levels$Passives[2])|>
                    dplyr::if_else("Passives"  ,"x_x")|>na_if("x_x"),
                  Promoters  = between(rating, df_levels$Promoters[1],df_levels$Promoters[2])|>
                    dplyr::if_else("Promoters" ,"x_x")|>na_if("x_x")
    )
  # Merge to single feature
  df_data <- df_data|>
    tidyr::pivot_longer(c("Detractors","Passives","Promoters"), values_drop_na = TRUE, names_to = "typ_nps")|>
    dplyr::mutate(value = NULL)
  return(df_data)
}


####################################
#' Get Net Promoter Score
#'
#' @name NPS
#' @description Calculates the Net Promoter Score for grouped or ungrouped data.
#' @details NPS = Ratio Promoters - Ratio Detractors,
#' whereas the Ratio of Promoters is calculated by count of Promoters / count total
#' and the the Ratio of Detractors is calculated by count of Detractors / count total.
#' If the \code{c_group} parameter is available the NPS score will be calculated for each group.
#' If the \code{df_levels} parameter is provided the level to find Promoters, Passives and Detractors can be defined by this definition.
#' The parameter \code{df_levels} is by standard: Detractors (1...6), Passives (7,8), Promoters (9,10) whereas the number shall be be integers only.
#' @param x dataframe
#' @param rating string identifying the column of the rating
#' @param c_group string with the column name to group the data frame.
#' @param df_levels data frame containing range for each type (Promoters, Passives, Detractors)
#' df_levels <- tibble::tibble(Detractors = c(1,6),
#'                     Passives   = c(7,8),
#'                     Promoters  = c(9,10))
#' @examples
#' library(tidyverse)
#' # Test data
#' df_data <- tibble::tibble(user   = c(letters[1:10],rep("a",5)),
#'                    Feedback = c(1:10,rep(10,5)),
#'                    Gruppe = c(rep("A",10),rep("B",5)))
#'
#' NPS(df_data, "Feedback", "Gruppe")
#' NPS(df_data, "Feedback")
#'
#' df_levels <- tibble::tibble(Detractors= c(1,3),Passives= c(4,4), Promoters= c(5,5))
#' df_data <- tibble::tibble(user   = c(letters[1:10],rep("a",5)),
#'                   Feedback = c(1:5,rep(5,10)),
#'                   Gruppe = c(rep("A",10),rep("B",5)))
#' NPS(df_data, "Feedback", "Gruppe",df_levels)
#' NPS(df_data, "Feedback",df_levels = df_levels)
#' @export

NPS <- function(x, rating , c_group , df_levels) {
  # Levels NPS
  if(missing(df_levels)) {
    df_levels <- tibble::tibble(Detractors = c(1,6),
                        Passives   = c(7,8),
                        Promoters  = c(9,10))
  }
  # exit function call if rating is not defined
  if(missing(rating)){
    stop("function error NPS(): \nrating argument is missing")
  }else{
    #copy data
    df_data <- x

    # check if c_groupe is availabel
    if(missing(c_group)){
      #change names of data frame
      names(df_data)<- tibble::tibble(name = names(x))|>
        dplyr::mutate(name_converted = dplyr::if_else(name==rating,"rating", name))|>
        dplyr::select(name_converted)|>
        dplyr::pull()
      # crate NPS_type
      df_data <- df_data|>
        NPS_type(df_data,df_levels = df_levels)
      #calculate NPS for not grouped data frames
      df_data <- df_data|>
        dplyr::filter(typ_nps == "Promoters")|>
        dplyr::summarise(n_Promoters = n())|>
        dplyr::bind_cols(df_data|>
                    dplyr::filter(typ_nps == "Detractors")|>
                    dplyr::summarise(n_Detractors = n())
        )|>
        dplyr::mutate(n_total = nrow(df_data),
               ratio_Promoters = n_Promoters/n_total,
               ratio_Detractors = n_Detractors/n_total,
               NPS = ratio_Promoters-ratio_Detractors)
      return(df_data)

    }else{
      #change names of data frame
      names(df_data)<- tibble::tibble(name = names(x))|>
        dplyr::mutate(name_converted = dplyr::if_else(name==rating,"rating", name),
               name_converted1 = dplyr::if_else(name==c_group, "c_group" , name_converted))|>
        dplyr::select(name_converted1)|>
        dplyr::pull()
      # crate NPS_type
      df_data <- df_data|>
        NPS_type(df_data,df_levels = df_levels)
      # calculate NPS Score for grouped Variable
      c_Gruppe <- df_data[,"c_group"]|>
        distinct()|>
        dplyr::pull()
      l_NPS <- list()
      for (i in 1:length(c_Gruppe)) {
        m <- df_data[,"c_group"] == c_Gruppe[i]
        m <- m[,1]
        df_temp <- df_data|>
          subset.data.frame(m)
        l_NPS[[i]] <- df_temp|>
          dplyr::filter(typ_nps == "Promoters")|>
          dplyr::summarise(n_Promoters = n())|>
          dplyr::bind_cols(df_temp|>
                      dplyr::filter(typ_nps == "Detractors")|>
                      dplyr::summarise(n_Detractors = n())
          )|>
          dplyr::mutate(n_total = nrow(df_temp),
                 ratio_Promoters = n_Promoters/n_total,
                 ratio_Detractors = n_Detractors/n_total,
                 NPS = ratio_Promoters-ratio_Detractors)
      }
      names(l_NPS) <- c_Gruppe
      df_data <- dplyr::bind_rows(l_NPS,.id = paste(c_group))
      names(df_data)[1] <- c_group
      return(df_data)
    }
  }
}

