###################################################################
#' Automatically creates a table of contents for a .Rmd file
#' @name r_tbl_contents
#' @description
#' Scans documents for headings and creates a table of contents (hyper linked). The returned string can directly be written as .Rmd file.
#' @details
#' The function argument is a string of a R markdown .Rmd file which can be loaded via \code{readLines("fileName.Rmd")}
#' @param  c_Rmd Rmd file string
#' @param  TOC_nb boolean to enable numbering for each heading
#' @return .Rmd file string
#' @examples
#' print(tbl_of_contents.Rmd)
#' c_rmd <- r_tbl_contents(tbl_of_contents.Rmd)
#' c_rmd
#' c_rmd <- r_tbl_contents(tbl_of_contents.Rmd, TRUE)
#' c_rmd
#' @export

r_tbl_contents <- function(c_Rmd, TOC_nb = FALSE) {
  # table of contents
  headings <- c_Rmd[stringr::str_detect(c_Rmd, "#")]|>stringr::str_remove_all("#")|>stringr::str_trim()
  headings

  # Hello
  df_data <- data.frame(index = 1:length(c_Rmd),
                        c_Rmd,
                        is.heading = stringr::str_detect(c_Rmd, "#")
  )

  df_data$c_add <- rep("",nrow(df_data))
  df_data$`#` <- stringr::str_detect(df_data$c_Rmd, "^#\\s")|>ifelse(1,0)
  df_data$`##` <- stringr::str_detect(df_data$c_Rmd, "^##\\s")|>ifelse(1,0)
  df_data$`###` <- stringr::str_detect(df_data$c_Rmd, "^###\\s")|>ifelse(1,0)
  df_data$`####` <- stringr::str_detect(df_data$c_Rmd, "^####\\s")|>ifelse(1,0)
  df_data$`#####` <- stringr::str_detect(df_data$c_Rmd, "^#####\\s")|>ifelse(1,0)
  df_data$`######` <- stringr::str_detect(df_data$c_Rmd, "^######\\s")|>ifelse(1,0)

  # Structure string
  c_add <- c("* ",
             "    + ",
             "        + ",
             "            + ",
             "                + ",
             "                    + "
  )
  heading_cnt <- rep(0,6)
  heading_cnt

  # loop through document and check for heading
  for (ii in 1:nrow(df_data)) {
    if(df_data$`#`[ii]){
      df_data$`#`[ii] <- heading_cnt[1] + 1
      heading_cnt[1] <- heading_cnt[1] + 1
      df_data$c_add[ii] <- c_add[1]
    }
    else if(df_data$`##`[ii]){
      df_data$`##`[ii] <- heading_cnt[2] + 1
      df_data$`#`[ii] <- heading_cnt[1]
      heading_cnt[2] <- heading_cnt[2] + 1
      df_data$c_add[ii] <- c_add[2]
    }
    else if(df_data$`###`[ii]){
      df_data$`###`[ii] <- heading_cnt[3] + 1
      df_data$`##`[ii] <- heading_cnt[1]
      df_data$`#`[ii] <- heading_cnt[1]
      heading_cnt[3] <- heading_cnt[3] + 1
      df_data$c_add[ii] <- c_add[3]
    }
    else if(df_data$`####`[ii]){
      df_data$`####`[ii] <- heading_cnt[4] + 1
      df_data$`###`[ii] <- heading_cnt[1]
      df_data$`##`[ii] <- heading_cnt[1]
      df_data$`#`[ii] <- heading_cnt[1]
      heading_cnt[4] <- heading_cnt[4] + 1
      df_data$c_add[ii] <- c_add[4]
    }
    else if(df_data$`#####`[ii]){
      df_data$`#####`[ii] <- heading_cnt[5] + 1
      df_data$`####`[ii] <- heading_cnt[1]
      df_data$`###`[ii] <- heading_cnt[1]
      df_data$`##`[ii] <- heading_cnt[1]
      df_data$`#`[ii] <- heading_cnt[1]
      heading_cnt[5] <- heading_cnt[5] + 1
      df_data$c_add[ii] <- c_add[5]
    }
    else if(df_data$`######`[ii]){
      df_data$`######`[ii] <- heading_cnt[6] + 1
      df_data$`#####`[ii] <- heading_cnt[1]
      df_data$`####`[ii] <- heading_cnt[1]
      df_data$`###`[ii] <- heading_cnt[1]
      df_data$`##`[ii] <- heading_cnt[1]
      df_data$`#`[ii] <- heading_cnt[1]
      heading_cnt[6] <- heading_cnt[6] + 1
      df_data$c_add[ii] <- c_add[6]
    }
  }

  # Create number string
  df_data$heading_nb <- df_data[,5:ncol(df_data)]|>
    apply(1,function(x){
      x <- ifelse(x == 0,NA,x)
      paste0(x[!is.na(x)], collapse = ".")
    })

  df_data$c_Rmd_ <- ifelse(df_data$is.heading,
                           paste0(df_data$c_Rmd," " ,df_data$heading_nb),
                           c_Rmd
  )

  # TOC without numbering
  df_data$TOC <- ifelse(df_data$is.heading,
                        paste0(df_data$c_add,"[",headings,"](#",headings,")"),
                        "")
  # TOC_ with numbering
  df_data$TOC_ <- ifelse(df_data$is.heading,
                         paste0(df_data$c_add,"[",headings," ",df_data$heading_nb,"](#",headings," ",df_data$heading_nb,")"),
                         "")
  cbind(df_data$c_Rmd,df_data$c_Rmd_)

  # create TOC string
  c_TOC <- df_data$TOC[df_data$is.heading]|>
    paste0("\n", collapse = "")

  # create TOC string
  c_TOC_ <- df_data$TOC_[df_data$is.heading]|>
    paste0("\n", collapse = "")

  # find position to insert table of contents
  for(ii in 1:length(c_Rmd)){
    if(stringr::str_detect(c_Rmd[ii],"#")) {
      c_start <- ii
      break
    }
  }
  if(!TOC_nb){
    # insert table off contents
    c_Rmd <- c(df_data$c_Rmd[1:(c_start - 1)],
               "# Tabel of Content",
               c_TOC,
               "\n",
               df_data$c_Rmd[c_start:nrow(df_data)])
    return(c_Rmd)
  }else{
    # insert table off contents
    c_Rmd <- c(df_data$c_Rmd_[1:(c_start - 1)],
               "# Tabel of Content",
               c_TOC_,
               "\n",
               df_data$c_Rmd_[c_start:nrow(df_data)])
    return(c_Rmd)
  }
}
