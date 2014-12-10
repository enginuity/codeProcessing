##@S This file contains generic text-prettifying functions



#' Creates a character vector of a varying amount of 'ch' characters
#'   
#' This is mainly used to space out text for pretty displaying (in printing dataframes)
#' Example: If v = c(5,1), returns c("     ", " ")
#' 
#' @param v Numeric vector
#' @param ch Character to repeat
#' 
#' @return Character vector of length(v)
#' 
#' @export
#' 
create_space_chars = function(v, ch = " ") {
  toreturn = as.character(v)
  for (j in 1:length(v)) {
    toreturn[j] = paste(rep(ch, times = v[j]), collapse = "")
  }
  return(toreturn)
}


## TODO: [Update] This function probably needs update to handle str_locate_all? is there a bug with this?
#' Marks the location of str_locate output 
#' 
#' @param strLoc Single entry str_locate output (corresponding to one vector, so a matrix with two columns, but any # rows)
#' @param marker What to use as indicator text
#' 
#' @return String with mostly spaces, except with matching locations marked
#' 
#' @export
#' 
mark_strlocate = function(strLoc, marker = "*") {
  res = str_dup(" ", max(strLoc[,2]))
  for(k in nrow(strLoc)) {
    str_sub(res, strLoc[k,1], strLoc[k,2]) <- str_dup(marker, strLoc[k,2] - strLoc[k,1]+1)
  }
  return(res)
}

