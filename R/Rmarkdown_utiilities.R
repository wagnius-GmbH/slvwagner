###################################################################
#' Automatically creates a table of contents for a .Rmd file
#' @name r_toc_for_Rmd
#' @description
#' Scans documents for headings and creates a table of contents (hyper linked). The returned string can directly be written as .Rmd file.
#' @details
#' The function argument is a string of a R markdown .Rmd file which can be loaded via \code{readLines("fileName.Rmd")}
#' @param c_Rmd Rmd file string
#' @param create_nb boolean to enable numbering for each heading.
#' @param nb_front boolean to have the numbering in front of each heading.
#' @return .Rmd file string
#' @examples
#' print(tbl_of_contents.Rmd)
#' c_rmd <- r_toc_for_Rmd(tbl_of_contents.Rmd)
#' c_rmd
#' c_rmd <- r_toc_for_Rmd(tbl_of_contents.Rmd, TRUE)
#' c_rmd
#' @export

r_toc_for_Rmd <- function(c_Rmd,create_nb = TRUE, nb_front = FALSE) {
  ##########################################################################
  # table of contents
  headings <- c_Rmd[stringr::str_detect(c_Rmd, "#")]|>stringr::str_remove_all("#")|>stringr::str_trim()

  # Hello
  df_data <- data.frame(index = 1:length(c_Rmd),
                        c_Rmd,
                        is.heading = stringr::str_detect(c_Rmd, "^#")
  )

  df_data$c_add <- rep("",nrow(df_data))
  df_data$`#` <- stringr::str_detect(df_data$c_Rmd, "^#\\s")|>ifelse(1,0)
  df_data$`##` <- stringr::str_detect(df_data$c_Rmd, "^##\\s")|>ifelse(1,0)
  df_data$`###` <- stringr::str_detect(df_data$c_Rmd, "^###\\s")|>ifelse(1,0)
  df_data$`####` <- stringr::str_detect(df_data$c_Rmd, "^####\\s")|>ifelse(1,0)
  df_data$`#####` <- stringr::str_detect(df_data$c_Rmd, "^#####\\s")|>ifelse(1,0)
  df_data$`######` <- stringr::str_detect(df_data$c_Rmd, "^######\\s")|>ifelse(1,0)

  ##########################################################################
  # Headings
  m <- df_data[df_data$is.heading,5:ncol(df_data)]

  ##########################################################################
  # find highest order Heading (Column)
  highest_order_jj <- 0
  for(ii in 1:6){
    if(sum(m[,ii]>0)){
      highest_order_jj <- ii
      break
    }
  }
  # print(highest_order_jj)

  # find index of highest order (row)
  highest_order_ii <- 0
  for(ii in 1:nrow(m)){
    if(m[ii,highest_order_jj] > 0){
      highest_order_ii <- ii
      break
    }
  }
  # print(highest_order_ii)

  ##########################################################################
  # Correct heading structure
  # How many columns to push pack
  if(highest_order_jj != ncol(m)){
    push_back <- highest_order_jj
  }
  # print(push_back)

  # Add columns according to push_back
  m_ <- switch (push_back,
                # m_ = m,
                m_ = cbind(m,p1=rep(0,nrow(m))),
                m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m))),
                m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m))),
                m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m))),
                m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m)),p5=rep(0,nrow(m))),
                m_ = cbind(m,p2=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m)),p5=rep(0,nrow(m)),p6=rep(0,nrow(m)))
  )
  # print(m_)

  ##########################################################################
  # shift all heading according to push_back and the first found highest order index
  for (ii in 1:nrow(m_)) {
    if (ii >= highest_order_ii) {
      temp <- m[ii,] |> as.matrix() |> as.vector()
      if (push_back == 1) {
        m_[ii,] <- c(0, temp)
      }
      else if (push_back == 2) {
        m_[ii,] <- c(0, 0, temp)
      }
      else if (push_back == 3) {
        m_[ii,] <- c(0, 0, 0, temp)
      }
      else if (push_back == 4) {
        m_[ii,] <- c(0, 0, 0, 0, temp)
      }
      else if (push_back == 5) {
        m_[ii,] <- c(0, 0, 0, 0, 0, temp)
      }
      else if (push_back == 6) {
        m_[ii,] <- c(0, 0, 0, 0, 0, 0, temp)
      }
    }
  }

  m_ <- m_[,(push_back+1):ncol(m_)]
  names(m_) <- c("#","##","###","####","#####","######")
  # print(m_)

  ##########################################################################
  # create structure number system

  # Heading structure counts
  heading_cnt <- rep(0,6)
  heading_cnt_ <- rep(0,6)
  last_heading_edited <- 0

  # Structure string
  c_add <- c("* ",
             "    + ",
             "        + ",
             "            + ",
             "                + ",
             "                    + ")
  c_add_structure <- 1:nrow(m_)

  m__ <- m_
  for (ii in 1:nrow(m_)) {
    for(jj in 1:6)
      if(m_[ii, jj] > 0){
        heading_cnt[jj] <- heading_cnt[jj] + 1
        if(last_heading_edited > jj) { # if heading order changes to higher order clear heading_cnt accordingly
          heading_cnt[(jj+1):length(heading_cnt)] <- 0
        }
        last_heading_edited <- jj
        break
      }
    m__[ii,] <- heading_cnt
    heading_cnt_ <- heading_cnt
    c_add_structure[ii] <- c_add[jj] # need to offset toc
  }

  # print(m__)

  ##########################################################################
  # create structure number
  c_nb <- m__|>
    apply(1, function(x){
      temp <- x[x>0]
      paste0(temp, collapse = ".")
    })

  ##########################################################################
  # create Heading and ancor
  c_Heading <- c_Rmd[df_data$is.heading]|> stringr::str_remove_all("#") |> stringr::str_trim()
  c_Heading_level <- c_Rmd[df_data$is.heading] |> stringr::str_split(" ", simplify = TRUE)
  c_Heading_level <- c_Heading_level[,1]

  if (create_nb) {
    if (nb_front) { # number system in front of heading
      c_ancor <- paste0(
        c_Heading_level," " , c_nb, " ", c_Heading ,
        "<a name=\"",
        c_nb, " ", c_Heading ,
        "\"></a>"
      )
      c_toc <- paste0("[", c_nb,  " ", c_Heading,"](#", c_nb," ", c_Heading, ")")
    } else {  # heading flowed by number system
      c_ancor <- paste0(
        c_Heading_level, " " , c_Heading, " ", c_nb,
        "<a name=\"",
        c_Heading, " ", c_nb,
        "\"></a>"
      )
      c_toc <- paste0("[", c_Heading, " ",c_nb,"](#", c_Heading," ",c_nb,")")
    }
  } else { # No numbering system / Do not Include number system
    c_ancor <- paste0(
      c_Heading_level, " ", c_Heading,
      "<a name=\"",
      c_Heading,
      "\"></a>"
    )
    c_toc <- paste0("[", c_Heading, "](#", c_Heading, ")")
  }

  # offset toc according to heading structure
  c_toc <- paste0(c_add_structure, c_toc)

  #########################################################################
  # Enhance headings
  df_data_ <- dplyr::left_join(df_data[,1:3],
                               data.frame(index = rownames(m__)|>as.integer(),
                                          c_ancor),
                               by = "index")

  df_data_$c_Rmd_ <- ifelse(df_data_$is.heading,df_data_$c_ancor, c_Rmd)

  #########################################################################
  # find position to insert table of contents
  check <- stringr::str_detect(c_Rmd, "(?:^#\\s|^##\\s)")
  check
  for (ii in 1:length(c_Rmd)) {
    if (check[ii]) {
      c_start <- ii
      break
    }
  }

  #########################################################################
  # Insert table of contents
  if (create_nb) {
    # insert table off contents
    c_Rmd <- c(df_data_$c_Rmd_ [1:(c_start - 1)],
               "# Tabel of Content",
               c_toc,
               "\n",
               df_data_$c_Rmd_[c_start:nrow(df_data)])
    return(c_Rmd)
  } else{
    # insert table off contents
    c_Rmd <- c(df_data_$c_Rmd_[1:(c_start - 1)],
               "# Tabel of Content",
               c_toc,
               "\n",
               df_data_$c_Rmd_[c_start:nrow(df_data)])
    return(c_Rmd)
  }
}
