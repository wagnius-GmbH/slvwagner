###################################################################
#' Automatically creates a table of contents for a .Rmd file
#' @name r_toc_for_Rmd
#' @description
#' Scans documents for headings (#) and creates a table of contents (hyper linked). The returned string can directly be written as .Rmd file. All code section will be excluded for the (#) search.
#' @details
#' The function argument is a string of a R markdown .Rmd file which can be read via \code{readLines("fileName.Rmd")}.
#' @param c_Rmd Rmd file string
#' @param create_nb boolean to enable numbering for each heading.
#' @param nb_front boolean to have the numbering in front of each heading.
#' @param create_top_link boolean to create link below each heading to jump back to the table of contents.
#' @return .Rmd file string
#' @examples
#' print(tbl_of_contents.Rmd)
#' c_rmd <- r_toc_for_Rmd(tbl_of_contents.Rmd, FALSE)
#' c_rmd
#' c_rmd <- r_toc_for_Rmd(tbl_of_contents.Rmd, TRUE)
#' c_rmd
#' @export

r_toc_for_Rmd <- function(c_Rmd,create_nb = TRUE, create_top_link = TRUE , nb_front = TRUE) {
  ##########################################################################
  # create dataframe to work with
  p <- "^```"

  df_data <- data.frame(index = 1:length(c_Rmd),
                        c_Rmd,
                        code_sections = lapply(c_Rmd, function(x) stringr::str_detect(x,p))|>unlist(),
                        is.heading = stringr::str_detect(c_Rmd, "^#")
  )

  # search and exclude code sections
  c_start_ii <- 0
  for (ii in 1:nrow(df_data)) {
    if (df_data$code_sections[ii] &  (c_start_ii != 0)) {
      df_data$code_sections[c_start_ii:ii] <- rep(TRUE,length(c_start_ii:ii))
      c_start_ii <- 0
    } else if (df_data$code_sections[ii]) {
      c_start_ii <- ii
    }
  }

  # remove heading in code section
  df_data$is.heading <- ifelse(df_data$code_sections, FALSE, df_data$is.heading)

  # Store headings
  # df_data$c_add <- rep("",nrow(df_data))
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
  # correct structure

  # check for headings in each column
  is.heading <- apply(m, 2,function(x){
    !sum(x)>0
  })

  # change structure so that the fist heading will be found in the first column
  if(sum(is.heading)>1) {
    for (ii in 1:length(is.heading)) {
      if(!is.heading[ii]) {
        push_front <- ii
        # Add columns according to push_back
        m_ <- switch (push_front,
                      m_ = m,
                      m_ = cbind(m[,2:6],p1=rep(0,nrow(m))),
                      m_ = cbind(m[,2:6],p1=rep(0,nrow(m[,2:5])),p2=rep(0,nrow(m))),
                      m_ = cbind(m[,3:6],p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m))),
                      m_ = cbind(m[,4:6],p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m))),
                      m_ = cbind(m[,5:6],p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m)),p5=rep(0,nrow(m)))
        )
        break
      }
    }
  }else{
    # Add columns according to push_back
    push_back <- push_back
    m_ <- switch (push_back,
                  m_ = m,
                  m_ = cbind(m,p1=rep(0,nrow(m))),
                  m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m))),
                  m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m))),
                  m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m))),
                  m_ = cbind(m,p1=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m)),p5=rep(0,nrow(m))),
                  m_ = cbind(m,p2=rep(0,nrow(m)),p2=rep(0,nrow(m)),p3=rep(0,nrow(m)),p4=rep(0,nrow(m)),p5=rep(0,nrow(m)),p6=rep(0,nrow(m)))
    )
  }


  ##########################################################################
  # find highest order Heading (Column)
  highest_order_jj <- 0
  for(ii in 1:6){
    if(sum(m_[,ii]>0)){
      highest_order_jj <- ii
      break
    }
  }
  # print(highest_order_jj)

  # find index of highest order (row)
  highest_order_ii <- 0
  for(ii in 1:nrow(m)){
    if(m_[ii,highest_order_jj] > 0){
      highest_order_ii <- ii
      break
    }
  }
  # print(highest_order_ii)

  # ##########################################################################
  # # Correct heading structure
  # shift all heading according to push_back and the first found highest order index
  if (slvwagner::r_is.defined(push_back)) {
    for (ii in 1:nrow(m_)) {
      if (ii >= highest_order_ii) {
        temp <- m[ii, ] |> as.matrix() |> as.vector()
        if (push_back == 1) {
          m_[ii, ] <- c(0, temp)
        }
        else if (push_back == 2) {
          m_[ii, ] <- c(0, 0, temp)
        }
        else if (push_back == 3) {
          m_[ii, ] <- c(0, 0, 0, temp)
        }
        else if (push_back == 4) {
          m_[ii, ] <- c(0, 0, 0, 0, temp)
        }
        else if (push_back == 5) {
          m_[ii, ] <- c(0, 0, 0, 0, 0, temp)
        }
        else if (push_back == 6) {
          m_[ii, ] <- c(0, 0, 0, 0, 0, 0, temp)
        }
      }
    }
  }

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
        "A_", # add some characters to ensure html links will work
        c_nb, "_", c_Heading ,
        "\"></a>",
        if(create_top_link)"\n[Tabel of Content](#Tabel of Content)\n"
      )
      c_toc <- paste0("[", c_nb,  " ", c_Heading,"](#A_", c_nb,"_", c_Heading, ")")
    } else {  # heading flowed by number system
      c_ancor <- paste0(
        c_Heading_level, " " , c_Heading, " ", c_nb,
        "<a name=\"",
        "A_", # add some characters to ensure html links will work
        c_Heading, " ", c_nb,
        "\"></a>",
        if(create_top_link)"\n[Tabel of Content](#Tabel of Content)\n"
      )
      c_toc <- paste0("[", c_Heading, " ",c_nb,"](#A_", c_Heading," ",c_nb,")")
    }
  } else { # No numbering system / Do not Include number system
    c_ancor <- paste0(
      c_Heading_level, " ", c_Heading,
      "<a name=\"",
      "A_", # add some characters to ensure html links will work
      c_Heading,
      "\"></a>",
      if(create_top_link)"\n[Tabel of Content](#Tabel of Content)\n"
    )
    c_toc <- paste0("[", c_Heading, "](#A_", c_Heading, ")")
  }

  # offset toc according to heading structure
  c_toc <- paste0(c_add_structure, c_toc)

  #########################################################################
  # Enhance headings
  df_data_ <- dplyr::left_join(df_data[,1:4],
                               data.frame(index = rownames(m__)|>as.integer(),
                                          c_ancor),
                               by = "index")

  df_data_$c_Rmd_ <- ifelse(df_data_$is.heading,df_data_$c_ancor, c_Rmd)

  #########################################################################
  # find position to insert table of contents
  check <- stringr::str_detect(c_Rmd, "(?:^#\\s|^##\\s)")

  for (ii in 1:length(c_Rmd)) {
    if (check[ii]) {
      c_start <- ii
      break
    }
  }

  ##########################################################################
  # create create top link
  c_toc_heading <- ifelse(create_top_link, "# Tabel of Content<a name=\"Tabel of Content\"></a>", "# Tabel of Content")

  #########################################################################
  # Insert table of contents
  if (create_nb) {
    # insert table off contents
    c_Rmd <- c(df_data_$c_Rmd_ [1:(c_start - 1)],
               c_toc_heading,
               c_toc,
               "\n",
               df_data_$c_Rmd_[c_start:nrow(df_data)])
    return(c_Rmd)
  } else{
    # insert table off contents
    c_Rmd <- c(df_data_$c_Rmd_[1:(c_start - 1)],
               c_toc_heading,
               c_toc,
               "\n",
               df_data_$c_Rmd_[c_start:nrow(df_data)])
    return(c_Rmd)
  }
}
