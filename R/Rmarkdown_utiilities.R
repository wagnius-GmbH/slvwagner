###################################################################
#' Automatically creates a table of contents for a .Rmd file
#' @name r_tbl_contents
#' @description
#' Scans documents for headings and creates a table of contents (hyper linked). The returned string can directly be written as .Rmd file.
#' @details
#' The function argument is a stirng of a R markdown .Rmd file which can be loaded via \code{readLines("fileName.Rmd")}
#'
#' @param  c_Rmd Rmd file string
#' @return .Rmd file string
#' @examples
#' print(tbl_of_contents.Rmd)
#' c_rmd <- r_tbl_contents(tbl_of_contents.Rmd)
#' c_rmd|>write("test.Rmd")
#' rmarkdown::render("test.Rmd", c("html_document"))
#' browseURL("test.html")
#' @export

r_tbl_contents <- function(c_Rmd) {
  # regex search pattern
  p <- "^#"

  # find structure
  c_struct <- c_Rmd[stringr::str_detect(c_Rmd, p)]
  c_struct

  # table of contents
  tbl_contents <- c_struct|>stringr::str_remove_all("#")|>stringr::str_split("<", simplify = T)|>stringr::str_trim()
  # create links
  tbl_contents <- paste0("[",tbl_contents,"]","(#",tbl_contents,")")

  # search string
  c_detect <- c("^#\\s",
                "^##\\s",
                "^###\\s",
                "^####\\s",
                "^#####\\s",
                "^######\\s"
  )

  # Structure string
  c_add <- c("* ",
             "    + ",
             "        + ",
             "            + ",
             "                + ",
             "                    + "
  )

  # create table of contents
  # first_heading <- TRUE
  for (ii in 1:length(tbl_contents)) {
    if(stringr::str_detect(c_struct[ii],c_detect[1]) ){
      tbl_contents[ii] <- paste0(c_add[1],tbl_contents[ii])
      # first_heading <- FALSE
    }
    else if(stringr::str_detect(c_struct[ii],c_detect[2])){
      tbl_contents[ii] <- paste0(c_add[2],tbl_contents[ii])
    }
    else if(stringr::str_detect(c_struct[ii],c_detect[3])){
      tbl_contents[ii] <- paste0(c_add[3],tbl_contents[ii])
    }
    else if(stringr::str_detect(c_struct[ii],c_detect[4])){
      tbl_contents[ii] <- paste0(c_add[4],tbl_contents[ii])
    }
    else if(stringr::str_detect(c_struct[ii],c_detect[5])){
      tbl_contents[ii] <- paste0(c_add[5],tbl_contents[ii])
    }
    else if(stringr::str_detect(c_struct[ii],c_detect[6])){
      tbl_contents[ii] <- paste0(c_add[6],tbl_contents[ii])
    }
  }

  # put anchor to all headings
  c_Rmd <- ifelse(stringr::str_detect(c_Rmd, p), paste0(c_Rmd, "<a name=\"",c_Rmd|>stringr::str_remove_all("#")|>stringr::str_trim(),"\"></a>"), c_Rmd)
  c_Rmd

  # find position to insert table of contents
  for(ii in 1:length(c_Rmd)){
    if(stringr::str_detect(c_Rmd[ii],p)) {
      c_start <- ii
      break
    }
  }

  # insert table off contents
  c_Rmd <- c(c_Rmd[1:(c_start-1)],
             "\n# Tabel of Content",
             tbl_contents, "\n",
             c_Rmd[c_start:length(c_Rmd)])
  return(c_Rmd)
}

# # r_tbl_contents
# # data for examples
# tbl_of_contents.Rmd <- readLines("files/input/tbl_of_contents.Rmd")
# tbl_of_contents.Rmd
# usethis:::use_data(tbl_of_contents.Rmd, internal = TRUE, overwrite = TRUE)
