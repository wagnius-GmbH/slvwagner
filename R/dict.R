###################################################################
#' Initialise Hash Enviroment
#'
#' @name dict_init_hash
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @param lenght lenght of the environment, e.g. nrow(data_frame)
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns Environment that can be uses as fast dictionary
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' # initialize hash
#' hash = dict_init_hash(nrow(df))
#' dict_assign_hash(df$key, df$value, hash)
#' dict_get_hash(c("ch", "it"), hash)
#' @export

dict_init_hash <- function(lenght)
{
  new.env(hash = TRUE, parent = emptyenv(), size = lenght)
}

###################################################################
#' Create a fast dictionary in R from with key and value
#'
#' @name dict_assign_hash
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns Environment hash that can be used as fast dictionary.
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' # initialize hash
#' hash = dict_init_hash(nrow(df))
#' # Assign key and values
#' dict_assign_hash(df$key, df$value, hash)
#' dict_get_hash(c("ch", "it"), hash)
#' @export
dict_assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))

###################################################################
#' Create a fast dictionary in R from data frame with key and value
#'
#' @name dict_get_hash
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns Environment hash that can be used as fast dictionary.
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' # initialize hash
#' hash <- dict_from_key_value(df)
#' dict_get_hash(c("ch", "it"), hash)
#' @export
dict_get_hash <- Vectorize(get, vectorize.args = "x")

###################################################################
#' Create a fast dictionary in R from data frame with key and value
#'
#' @name dict_exists_hash
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns Environment hash that can be used as fast dictionary.
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' hash <- dict_from_key_value(df)
#' dict_exists_hash(c("ch", "it"), hash)
#' @export
dict_exists_hash <- Vectorize(exists, vectorize.args = "x")


###################################################################
#' Create a fast dictionary in R from data frame with key and value
#'
#' @name dict_from_key_value
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @param df data frame with column 1 = key and column 2 = value
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns Environment hash that can be used as fast dictionary.
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' hash <- dict_from_key_value(df)
#' dict_get_hash(c("ch", "it"), hash)
#' @export

dict_from_key_value <- function(df)
  {
  # initialize hash
  hash = new.env(hash = TRUE, parent = emptyenv(), size = nrow(df))
  # assign values to keys
  dict_assign_hash(df[,1], df[,2], hash)
  return(hash)
  }
