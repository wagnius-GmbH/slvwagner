###################################################################
#' Automatically creates a table of contents for a .Rmd file
#' @name r_toc_for_Rmd
#' @description
#' Scans documents for headings (#) and creates a table of contents (hyper linked). The returned string can directly be written as .Rmd file.
#' All code section will be excluded for the (#) search.
#' @details
#' The function argument is a string of a R markdown .Rmd file which can be read via \code{readLines("fileName.Rmd")}.
#' @param toc_heading_string title string for TOC, e.g "Inhaltsverzeichnis", default is "Table of contents".
#' @param c_Rmd Rmd file string
#' @param create_nb boolean to enable numbering for each heading.
#' @param nb_front boolean to have the numbering in front of each heading.
#' @param create_top_link boolean to create link below each heading to jump back to the table of contents.
#' @param set_first_heading_level set first found heading level to heading 1
#' @param pagebreak_level Automatically add page breaks before new heading. Set the level to insert page breaks to e.g. c("non","1","2","3","4","5","6")
#' @return .Rmd file string
#' @examples
#' print(tbl_of_contents.Rmd)
#' c_rmd <- r_toc_for_Rmd(tbl_of_contents.Rmd, create_nb = FALSE)
#' c_rmd
#' c_rmd <- r_toc_for_Rmd(tbl_of_contents.Rmd,toc_heading_string = "Inhaltsverzeichnis", create_nb = TRUE)
#' c_rmd
#' @export

r_toc_for_Rmd <- function(
    c_Rmd,
    toc_heading_string = "Table of Contents" ,
    create_nb = TRUE, create_top_link = TRUE , nb_front = TRUE, set_first_heading_level = FALSE,
    pagebreak_level = "non"
)
{
  ##########################################################################
  # create data frame to work with
  df_data <- create_df(c_Rmd)

  ##########################################################################
  # Headings
  m <- df_data[df_data$is.heading, 5:ncol(df_data)]

  ##########################################################################
  # Analyze heading structure
  heading_struct <- m|>
    apply(2, function(x) {
      sum(x)>0
    })

  # highest order heading column index
  for (ii in 1:ncol(m)) {
    if(heading_struct[ii]) {
      highest_order_jj <- ii
      break
    }
  }

  # highest order heading row index
  for (ii in 1:nrow(m)) {
    if(m[ii,highest_order_jj]) {
      highest_order_ii <- ii
      break
    }
  }

  # find first heading
  for (ii in 1:6) {
    if(m[1,ii]>0){
      first_heading_column <- ii
    }
  }

  ##########################################################################
  # correct heading structure
  c_names <- c("#","##","###","####","#####","######")

  if(highest_order_jj != first_heading_column){
    # correct structure
    temp <- m[1:(highest_order_ii-1),first_heading_column:6]
    temp
    temp <- switch (first_heading_column ,
                    temp = temp,
                    temp = cbind(temp,p1 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp)),p5 = rep(0,nrow(temp))),
                    temp = cbind(temp,p1 = rep(0,nrow(temp)),p2 = rep(0,nrow(temp)),p3 = rep(0,nrow(temp)),p4 = rep(0,nrow(temp)),p5 = rep(0,nrow(temp)),p6 = rep(0,nrow(temp))),
    )
    temp

    temp1 <- m[highest_order_ii:nrow(m),highest_order_jj:6]
    temp1 <- switch (highest_order_jj,
                     temp1 = temp1,
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1)),p5 = rep(0,nrow(temp1))),
                     temp1 = cbind(temp1,p1 = rep(0,nrow(temp1)),p2 = rep(0,nrow(temp1)),p3 = rep(0,nrow(temp1)),p4 = rep(0,nrow(temp1)),p5 = rep(0,nrow(temp1)),p6 = rep(0,nrow(temp1))),
    )
    names(temp1) <- c_names[highest_order_jj:6]
    names(temp) <- c_names[highest_order_jj:6]
    m_ <- rbind(temp,temp1)
  }else{
    if(highest_order_jj>0){ # remove not populated columns
      m_ <- switch (highest_order_jj,
                    m_ = m,
                    m_ = cbind(m[,2:6],p1 = rep(0,nrow(m))),
                    m_ = cbind(m[,3:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m))),
                    m_ = cbind(m[,4:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m))),
                    m_ = cbind(m[,5:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m)),p4 = rep(0,nrow(m))),
                    m_ = cbind(m[,6:6],p1 = rep(0,nrow(m)),p2 = rep(0,nrow(m)),p3 = rep(0,nrow(m)),p4 = rep(0,nrow(m)),p5 = rep(0,nrow(m)))
      )
    }else{
      m_ <- m
    }
  }
  m_

  ##########################################################################
  # create structure number system
  # Heading structure counts
  heading_cnt <- rep(0, 6)
  heading_cnt_ <- rep(0, 6)
  last_heading_edited <- 0

  # Structure string
  c_add <- c("* ",
             "    + ",
             "        + ",
             "            + ",
             "                + ",
             "                    + ")

  c_add_structure <- 1:nrow(m_)
  column_cnt <- 0
  m__ <- m_
  c_Heading_level <- 1:nrow(m_)
  for (ii in 1:nrow(m_)) {
    for (jj in 1:6)
      if (m_[ii, jj] > 0) {
        heading_cnt[jj] <- heading_cnt[jj] + 1
        if (last_heading_edited > jj) {
          # if heading order changes to higher order clear heading_cnt accordingly
          heading_cnt[(jj + 1):length(heading_cnt)] <- 0

        }
        last_heading_edited <- jj
        break
      }
    m__[ii, 1:6] <- heading_cnt
    heading_cnt_ <- heading_cnt
    if(set_first_heading_level){
      c_Heading_level[ii] <- c_names[jj]
    }else{
      c_Heading_level[ii] <- c_names[jj + (highest_order_jj-1)]
    }
    c_add_structure[ii] <- c_add[jj]

  }

  ##########################################################################
  # create structure number
  c_nb <- m__ |>
    apply(1, function(x) {
      temp <- x[x > 0]
      paste0(temp, collapse = ".")
    })

  ##########################################################################
  # create link link to table of contents
  c_top_link <-  paste0("\n[", toc_heading_string, "](#", toc_heading_string, ")\n")
  c_top_link

  ##########################################################################
  c_Heading <- c_Rmd[df_data$is.heading]|>stringr::str_remove_all("#")|>stringr::str_trim()
  c_Heading

  ##########################################################################
  # create anchor
  if (create_nb) {
    if (nb_front) { # number system in front of heading
      c_anchor <- paste0(
        c_Heading_level," " , c_nb, " ", c_Heading ,
        "<a name=\"",
        "A_", # add some characters to ensure html links will work
        c_nb, "_", c_Heading ,
        "\"></a>",
        if(create_top_link) c_top_link
      )
      c_toc <- paste0("[", c_nb,  " ", c_Heading,"](#A_", c_nb,"_", c_Heading, ")")
    } else {  # heading flowed by number system
      c_anchor <- paste0(
        c_Heading_level, " " , c_Heading, " ", c_nb,
        "<a name=\"",
        "A_", # add some characters to ensure html links will work
        c_Heading, " ", c_nb,
        "\"></a>",
        if(create_top_link) c_top_link
      )
      c_toc <- paste0("[", c_Heading, " ",c_nb,"](#A_", c_Heading," ",c_nb,")")
    }
  } else { # No numbering system / Do not Include number system
    c_anchor <- paste0(
      c_Heading_level, " ", c_Heading,
      "<a name=\"",
      "A_", # add some characters to ensure html links will work
      c_Heading,
      "\"></a>",
      if(create_top_link) c_top_link
    )
    c_toc <- paste0("[", c_Heading, "](#A_", c_Heading, ")")
  }

  # format toc according to found heading structure
  c_toc <- paste0(c_add_structure, c_toc)

  #########################################################################
  # Enhance headings
  df_data_ <- dplyr::left_join(df_data[, 1:4],
                               data.frame(index = rownames(m__) |> as.integer(),
                                          c_anchor),
                               by = "index")

  df_data_$c_Rmd_ <-  ifelse(!is.na(df_data_$c_anchor), df_data_$c_anchor, c_Rmd)



  #########################################################################
  # create TOC
  highest_order_jj <- ifelse(set_first_heading_level, 1, highest_order_jj)
  c_toc_link <- switch(highest_order_jj,
                       paste0(c_names[1]," ",toc_heading_string),
                       paste0(c_names[2]," ",toc_heading_string),
                       paste0(c_names[3]," ",toc_heading_string),
                       paste0(c_names[4]," ",toc_heading_string),
                       paste0(c_names[5]," ",toc_heading_string),
                       paste0(c_names[6]," ",toc_heading_string)
  )

  c_toc_link <- ifelse(create_top_link,
                       paste0(c_toc_link, "<a name=\"", toc_heading_string, "\"></a>"),
                       c_toc_link)

  #########################################################################
  # find position to insert table of contents
  check <- stringr::str_detect(c_Rmd, "---")
  c_start <- 1
  cnt <- 0

  for (ii in 1:length(c_Rmd)) {
    if (check[ii]) {
      c_start <- ii
      cnt <- cnt + 1
      if(cnt == 2) break
    }
  }

  #########################################################################
  # Insert table of contents
  c_Rmd <- c(df_data_$c_Rmd_ [1:(c_start)],
             c_toc_link,
             c_toc,
             "\n",
             df_data_$c_Rmd_[(c_start+1):nrow(df_data)]
  )

  #########################################################################
  # Insert page breaks

  #create data frame to work with
  df_data <- create_df(c_Rmd)

  # Headings
  m <- df_data[df_data$is.heading, 5:ncol(df_data)]

  # Analyze heading structure
  heading_struct <- m|>
    apply(2, function(x) {
      sum(x)>0
    })

  # highest order heading column index
  for (ii in 1:ncol(m)) {
    if(heading_struct[ii]) {
      highest_order_jj <- ii
      break
    }
  }

  if(highest_order_jj > 1 & pagebreak_level != "non") pagebreak_level <- (pagebreak_level|>as.integer() +  highest_order_jj - 1)|>as.character()

  m_pb <- switch (
    pagebreak_level,
    "non" = FALSE,
    "1" = m[, 1:1]|>matrix(dimnames =list(row.names(m),"#"))|>as.data.frame(),
    "2" = m[, 1:2],
    "3" = m[, 1:3],
    "4" = m[, 1:4],
    "5" = m[, 1:5],
    "6" = m[, 1:6],
  )

  # add html page break tag
  if(is.data.frame(m_pb)) {
    for (ii in 2:nrow(m_pb)) {
      for (jj in 1:ncol(m_pb)) {
        if (m_pb[ii, jj] > 0) {
          index <- row.names(m_pb)[ii] |> as.integer()
          c_Rmd[index] <-
            paste0("\n",
                   "\\newpage",
                   # "<div style=\"page-break-after: always\"></div>",
                   "\n",
                   c_Rmd[index])
        }
      }
    }
  }

  return(c_Rmd)
}


create_df <- function(c_Rmd) {
  p <- "^```"
  df_data <- data.frame(
    index = 1:length(c_Rmd),
    c_Rmd,
    code_sections = lapply(c_Rmd, function(x)
      stringr::str_detect(x, p)) |> unlist(),
    is.heading = stringr::str_detect(c_Rmd, "^#")
  )

  # search and exclude code sections
  c_start_ii <- 0
  for (ii in 1:nrow(df_data)) {
    if (df_data$code_sections[ii] &  (c_start_ii != 0)) {
      df_data$code_sections[c_start_ii:ii] <-
        rep(TRUE, length(c_start_ii:ii))
      c_start_ii <- 0
    } else if (df_data$code_sections[ii]) {
      c_start_ii <- ii
    }
  }

  # remove heading in code section
  df_data$is.heading <- ifelse(df_data$code_sections, FALSE, df_data$is.heading)

  # Store headings
  df_data$`#` <- stringr::str_detect(df_data$c_Rmd, "^#\\s") |> ifelse(1, 0)
  df_data$`##` <-  stringr::str_detect(df_data$c_Rmd, "^##\\s") |> ifelse(1, 0)
  df_data$`###` <- stringr::str_detect(df_data$c_Rmd, "^###\\s") |> ifelse(1, 0)
  df_data$`####` <- stringr::str_detect(df_data$c_Rmd, "^####\\s") |> ifelse(1, 0)
  df_data$`#####` <- stringr::str_detect(df_data$c_Rmd, "^#####\\s") |> ifelse(1, 0)
  df_data$`######` <- stringr::str_detect(df_data$c_Rmd, "^######\\s") |> ifelse(1, 0)
  return(df_data)
}



###################################################################
#' Automatically creates a table of contents for all found .Rmd files in actual working directory
#' @name r_tocForRmdFiles
#' @param render_type choose the render type e.g. c("html_document","word_document","pdf_document")
#' @param toc_heading_string title string for TOC, e.g "Inhaltsverzeichnis", default is "Table of contents".
#' @param pagebreak_level Automatically add page breaks before new heading. Set the level to insert page breaks to e.g. c("non","1","2","3","4","5","6")
#' @param create_nb boolean to enable numbering for each heading.
#' @param nb_front boolean to have the numbering in front of each heading.
#' @param set_first_heading_level set first found heading level to heading 1
#' @param delete_Rmd_file boolean to delete the created .Rmd files.
#' @examples
#' write(tbl_of_contents.Rmd)
#' r_tocForRmdFiles()
#' print(getwd())
#' @export

r_tocForRmdFiles <- function(
    render_type = c("html_document"),
    toc_heading_string = "Table of Contents",
    pagebreak_level = "non",
    create_nb = TRUE,
    nb_front = TRUE,
    set_first_heading_level = FALSE,
    delete_Rmd_file = TRUE
)
{

  ################################################################################################################
  # html documents
  if(sum(render_type == "html_document") > 0) {
    c_FileName <- list.files()
    c_FileName <-  c_FileName[c_FileName |> stringr::str_detect(".Rmd$") &
                                !c_FileName |> stringr::str_detect("_.Rmd")] |> stringr::str_remove(".Rmd$")

    if (length(c_FileName) == 0)stop(paste0("\nNo \".Rmd\" files found in current working directory:\n", getwd()))

    c_FileExtention <- ".Rmd"
    c_file <-  paste0(c_FileName |> stringr::str_replace_all(" ", "_"))
    c_Rmd <- paste0(getwd(), "/", c_file, "_", c_FileExtention)

    dir.create("output/") |> suppressWarnings()
    ####################
    # create Rmd files
    for (ii in 1:length(c_FileName)) {
      paste0(c_FileName[ii], c_FileExtention) |>
        readLines() |>
        r_toc_for_Rmd(toc_heading_string = toc_heading_string,
                      create_nb = create_nb,
                      nb_front = nb_front,
                      set_first_heading_level = set_first_heading_level
        ) |>
        writeLines(c_Rmd[ii])
    }
    ####################
    # render files
    for (ii in 1:length(c_FileName)) {
      # render files
      try(rmarkdown::render(c_Rmd[ii],
                            c("html_document"),
                            output_dir = paste0(getwd(), "/output")))
    }

    c_FileName <- list.files()
    c_FileName <- c_FileName[c_FileName |> stringr::str_detect("_.Rmd")]

    ii <- 1
    for (ii in 1:length(c_FileName)) {
      file.remove(c_FileName[ii])
    }
  }

  ################################################################################################################
  # word and PDF
  c_FileName <- list.files()
  c_FileName <- c_FileName[c_FileName|>stringr::str_detect(".Rmd$") & !c_FileName|>stringr::str_detect("_.Rmd")]|>stringr::str_remove(".Rmd$")
  c_FileExtention <- ".Rmd"
  c_file <-  paste0(c_FileName|>stringr::str_replace_all(" ","_"))
  c_Rmd <- paste0(getwd(),"/",c_file,"_",c_FileExtention)

  ii <- 1
  for (ii in 1:length(c_FileName)) {
    # create Rmd files
    paste0(c_FileName[ii],c_FileExtention)|>
      readLines()|>
      r_toc_for_Rmd(toc_heading_string = toc_heading_string,
                    pagebreak_level = pagebreak_level,
                    create_top_link = FALSE,
                    create_nb = create_nb,
                    nb_front = nb_front,
                    set_first_heading_level = set_first_heading_level
      )|>
      writeLines(c_Rmd[ii])
  }

  #########################
  # render files word files
  if("word_document" %in% render_type) {
    for (ii in 1:length(c_FileName)) {
      try(rmarkdown::render(c_Rmd[ii],
                            c("word_document"),
                            output_dir = paste0(getwd(), "/output")))
    }
  }
  #########################
  # render files pdf files
  if("pdf_document" %in% render_type) {
    for (ii in 1:length(c_FileName)) {
      # render files
      try(rmarkdown::render(c_Rmd[ii],
                            c("pdf_document"),
                            output_dir = paste0(getwd(), "/output")))
    }
  }

  # Delete .Rmd files
  if (delete_Rmd_file) {
    c_FileName <- list.files()
    c_FileName <- c_FileName[c_FileName |> stringr::str_detect("_.Rmd")]
    for (ii in 1:length(c_FileName)) {
      file.remove(c_FileName[ii])
    }
  }
}

