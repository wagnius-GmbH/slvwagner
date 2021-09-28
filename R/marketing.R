####################################
#' Get Net Promoter Score
#'
#' @name get_NPS
#' @description Get Net Promoter Score of rating 1...10. NPS = Ratio Promoters - Ratio Detractors,
#' whereas the Ratio of Promoters is calculated by count of Promoters / count total
#' and the the Ratio of Detractors is calculated by count of Detractors / count total.
#' @param rating string with column name of data frame containing the vector of the ratings
#' @param c_group string with column name to group the data frame and calcultae NPS for each group
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
#' @export

get_NPS <- function(x, rating , c_group ) {
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
        mutate(Detractors = between(paste(rating), 0, 6)%>%
                 if_else("Detractors","x_x")%>%na_if("x_x"),
               Passives   = between(paste(rating), 7, 8)%>%
                 if_else("Passives"  ,"x_x")%>%na_if("x_x"),
               Promoters  = between(paste(rating), 9,10)%>%
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
        mutate(Detractors = between(paste(rating), 0, 6)%>%
                 if_else("Detractors","x_x")%>%na_if("x_x"),
               Passives   = between(paste(rating), 7, 8)%>%
                 if_else("Passives"  ,"x_x")%>%na_if("x_x"),
               Promoters  = between(paste(rating), 9,10)%>%
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

