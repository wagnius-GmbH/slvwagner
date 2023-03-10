###################################################################
#' Initialize fast dictionary environment
#'
#' @name dict_init
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @param lenght lenght of the environment, e.g. nrow(data_frame)
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns A initialized environment that can be assigned with keys and values
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' # initialize hash
#' hash = dict_init(nrow(df))
#' dict_assign_key_values(df$key, df$value, hash)
#' dict_get_values(c("ch", "it"), hash)
#' @export

dict_init <- function(lenght)
{
  new.env(hash = TRUE, parent = emptyenv(), size = lenght)
}

###################################################################
#' Assigne key and value to fast dictionary
#'
#' @name dict_assign_key_values
#' @usage 1. Argument:vector key. 2. Argument: vector value, 3. Argument:The Environment.
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns Environment hashed, that can be used as fast dictionary.
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' # initialize hash
#' hash = dict_init(nrow(df))
#' # Assign key and values
#' dict_assign_key_values(df$key, df$value, hash)
#' dict_get_values(c("ch", "it"), hash)
#' @export
dict_assign_key_values <- Vectorize(assign, vectorize.args = c("x", "value"))

###################################################################
#' Get values from fast dictionary
#'
#' @name dict_get_values
#' @usage 1. Argument:vector to check, 2. Argument: The Environment.
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns named vector
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' # initialize hash
#' hash <- dict_from_data.frame(df)
#' dict_get_values(c("ch", "it"), hash)
#' @export
dict_get_values <- Vectorize(get, vectorize.args = "x")

###################################################################
#' Check if key is in dictionary
#'
#' @name dict_exists_key
#' @usage 1. Argument:vector to check, 2. Argument: The Environment.
#' @description The reason for using dictionaries in the first place is performance.
#' Although it is correct that you can use named vectors and lists for the task,
#' the issue is that they are becoming quite slow and memory hungry with more data.
#' Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
#' environments with the option hash = TRUE
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns Vector True/False
#' @examples
#' df <- data.frame(key   = c("ch","se","de","it"),
#'                  value = c(41L,46L,49L,39L))
#' hash <- dict_from_data.frame(df)
#' dict_exists_key(c("ch", "it", "xx"), hash)
#' @export
dict_exists_key <- Vectorize(exists, vectorize.args = "x")


###################################################################
#' Create a fast dictionary from data frame
#'
#' @name dict_from_data.frame
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
#' hash <- dict_from_data.frame(df)
#' dict_get_values(c("ch", "it"), hash)
#' @export

dict_from_data.frame <- function(df)
  {
  df <- as.data.frame(df)
  # initialize hash
  hash = new.env(hash = TRUE, parent = emptyenv(), size = nrow(df))
  # assign values to keys
  dict_assign_key_values(df[,1], df[,2], hash)
  return(hash)
  }


###################################################################
#' Update key/value pairs of fast dictionary
#'
#' @name dict_update
#' @usage This function can update, add key/value pairs. Check the examples for more information.
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
#' hash <- dict_from_data.frame(df)
#' dict_get_values(c("ch", "it"), hash)
#' hash <- dict_update(data.frame(key = c("ch","it","test"), value = c(4.1, 0.39,0)), hash)
#' dict_get_values(ls(hash), hash)
#'
#' @export

dict_update <- function(df, dict)
  {
  df <- as.data.frame(df)
  if(nrow(df) == 1){
    dict[[df[1,1]]] <- df[1,2]
  }else{
    for (ii in 1:nrow(df)) {
      dict[[df[ii,1]]] <- df[ii,2]
    }
  }
  return(dict)
}

