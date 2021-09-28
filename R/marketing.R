####################################
#' Get Net Promoter Score
#'
#' @name get_NPS
#' @description Calculates the Net Promoter Score.
#' @details NPS = Ratio Promoters - Ratio Detractors,
#' whereas the Ratio of Promoters is calculated by count of Promoters / count total
#' and the the Ratio of Detractors is calculated by count of Detractors / count total.
#' If the \code{c_group} parameter is available the NPS score will be calculated for each group.
#' If the \code{df_levels} parameter is provided the level to find Promoters, Passives and Detractors can be defined by this definition.
#' @param rating string with column name of the rating.
#' @param c_group string with the column name to group the data frame.
#' @param df_levels data frame containing the rating range for each type (Promoters, Passives, Detractors).
#' @examples
#' library(tidyverse)
#' # Test data
#' df_data <- tibble(user   = c(letters[1:10],rep("a",5)),
#'                    Feedback = c(1:10,rep(10,5)),
#'                    Gruppe = c(rep("A",10),rep("B",5)))%>%
#'   mutate(user = factor(user))
#'
#' get_NPS(df_data, "Feedback", "Gruppe")
#' get_NPS(df_data, "Feedback")
#'
#' df_levels <- tibble(Detractors= c(1,3),Passives= c(4,4), Promoters= c(5,5))
#' df_data <- tibble(user   = c(letters[1:10],rep("a",5)),
#'                   Feedback = c(1:5,rep(5,10)),
#'                   Gruppe = c(rep("A",10),rep("B",5)))
#' get_NPS(df_data, "Feedback", "Gruppe",df_levels)
#' get_NPS(df_data, "Feedback",df_levels = df_levels)
#' @export

get_NPS <- function(x, rating , c_group , df_levels) {
  # Levels NPS
  if(missing(df_levels)) {
    df_levels <- tibble(Detractors = c(1,6),
                       Passives   = c(7,8),
                       Promoters  = c(9,10))
  }
  # exit function call if rating is not defined
  if(missing(rating)){
    message("function error get_NPS(): \nrating argument is missing")
  }else{
    #copy data
    df_data <- x

    # check if c_groupe is availabel
    if(missing(c_group)){
      #change names of data frame
      names(df_data)<- tibble(name = names(x))%>%
        mutate(name_converted = if_else(name==rating,"rating", name))%>%
        select(name_converted)%>%
        pull()

      # Create feature Promoters, Passives and Detractors
      df_data <- df_data%>%
        mutate(Detractors = between(paste(rating), df_levels$Detractors[1], df_levels$Detractors[2])%>%
                 if_else("Detractors","x_x")%>%na_if("x_x"),
               Passives   = between(paste(rating), df_levels$Passives[1], df_levels$Passives[2])%>%
                 if_else("Passives"  ,"x_x")%>%na_if("x_x"),
               Promoters  = between(paste(rating), df_levels$Promoters[1],df_levels$Promoters[2])%>%
                 if_else("Promoters" ,"x_x")%>%na_if("x_x")
        )
      # Merge to single feature
      df_data <- df_data%>%
        pivot_longer(c("Detractors","Passives","Promoters"), values_drop_na = TRUE, names_to = "typ_nps")%>%
        mutate(value = NULL)
      #calculate NPS for not grouped data frames
      df_data <- df_data%>%
        filter(typ_nps == "Promoters")%>%
        summarise(n_Promoters = n())%>%
        bind_cols(df_data%>%
                    filter(typ_nps == "Detractors")%>%
                    summarise(n_Detractors = n())
        )%>%
        mutate(n_total = nrow(df_data),
               ratio_Promoters = n_Promoters/n_total,
               ratio_Detractors = n_Detractors/n_total,
               NPS = ratio_Promoters-ratio_Detractors)
      return(df_data)

    }else{
      #change names of data frame
      names(df_data)<- tibble(name = names(x))%>%
        mutate(name_converted = if_else(name==rating,"rating", name),
               name_converted1 = if_else(name==c_group, "c_group" , name_converted))%>%
        select(name_converted1)%>%
        pull()
      # Create feature Promoters, Passives and Detractors
      df_data <- df_data%>%
        mutate(Detractors = between(paste(rating), df_levels$Detractors[1], df_levels$Detractors[2])%>%
                 if_else("Detractors","x_x")%>%na_if("x_x"),
               Passives   = between(paste(rating), df_levels$Passives[1], df_levels$Passives[2])%>%
                 if_else("Passives"  ,"x_x")%>%na_if("x_x"),
               Promoters  = between(paste(rating), df_levels$Promoters[1],df_levels$Promoters[2])%>%
                 if_else("Promoters" ,"x_x")%>%na_if("x_x")
        )
      # Merge to single feature
      df_data <- df_data%>%
        pivot_longer(c("Detractors","Passives","Promoters"), values_drop_na = TRUE, names_to = "typ_nps")%>%
        mutate(value = NULL)

      # calculate NPS Score for grouped Variable
      c_Gruppe <- df_data[,"c_group"]%>%
        distinct()%>%
        pull()

      l_NPS <- list()
      for (i in 1:length(c_Gruppe)) {
        m <- df_data[,"c_group"] == c_Gruppe[i]
        m <- m[,1]

        df_temp <- df_data%>%
          subset.data.frame(m)

        df_temp

        l_NPS[[i]] <- df_temp%>%
          filter(typ_nps == "Promoters")%>%
          summarise(n_Promoters = n())%>%
          bind_cols(df_temp%>%
                      filter(typ_nps == "Detractors")%>%
                      summarise(n_Detractors = n())
          )%>%
          mutate(n_total = nrow(df_temp),
                 ratio_Promoters = n_Promoters/n_total,
                 ratio_Detractors = n_Detractors/n_total,
                 NPS = ratio_Promoters-ratio_Detractors)
      }
      names(l_NPS) <- c_Gruppe
      df_data <- bind_rows(l_NPS,.id = paste(c_group))
      names(df_data)[1] <- c_group
      return(df_data)
    }
  }
  return(NULL)
}

